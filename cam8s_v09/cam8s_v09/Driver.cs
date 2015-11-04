
// --------------------------------------------------------------------------------
// ASCOM Camera driver for cam8s v.0.9
// Edit Log:
// Date			Who	Vers	Description
// -----------	---	-----	-------------------------------------------------------
// 24-sep-2014  VSS 0.5     Initial release (based on cam8 v.0.5)
// 27-nov-2014  VSS 0.55    Fixed minor bugs with saving gain/offset settings
// 08-mar-2015  VSS 0.6     Some code refactoring + added TEC control
// 10-apr-2015  VSS 0.7     Bugfix
// 20-aug-2015  VSS 0.8     Stob/Abort exposure xception type fix, main update in delphi dll
// 04-nov-2015  VSS 0.9     Fix compatibility with Nebulosity
// --------------------------------------------------------------------------------


// This is used to define code in the template that is specific to one class implementation
// unused code canbe deleted and this definition removed.
#define Camera

#define Camera

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Text;
using System.Runtime.InteropServices;

using ASCOM;
using ASCOM.Astrometry;
using ASCOM.Astrometry.AstroUtils;
using ASCOM.Utilities;
using ASCOM.DeviceInterface;
using System.Globalization;
using System.Collections;
//for COM PORT
using System.IO.Ports;
//for pauses
using System.Threading;

namespace ASCOM.cam8s_v09
{
    /// <summary>
    /// ASCOM Camera Driver for cam8s_v09.
    /// </summary>
    [Guid("0f9a0d8d-f77d-42c0-a9be-674480ce403d")]
    [ClassInterface(ClassInterfaceType.None)]

    public class TECControl
    {
        private const int baudrate = 9600;
        private const byte rxBufferSize = 11;
        private const byte responcePacketSize = 11;
        private const byte infoPacketSize = 6;
        private const byte hwRevision = 0x01;
        private const byte swRevision = 0x01;
        private const byte sensorCount = 0x02;
        private const byte setCount = 0x01;
        private const double minSetTemp = -50.0;
        private const double maxSetTemp = 50.0;
        private const int tempOffset = 128;
        private const byte tecOn = 0x01;
        private const byte tecOff = 0x00;

        private byte[] infoCmd = { 0x69, 0x9f };
        private byte[] getCmd = { 0x67, 0x80 };
        private byte[] setCmd = { 0x73, 0x00, 0x00, 0x00 };
        private byte[] pwmCmd = { 0x70, 0x00, 0x00 };

        private SerialPort tecComPort;
        private double tecCCDTemp = 25.0;
        private double tecHeatsinkTemp = 25.0;
        private double tecSetTemp = 25.0;
        private double tecCoolerPower = 0.0;
        private int tecErrorCode = 0;
        private bool tecCoolenOn = false;
        private bool tecIsConnected = false;

        private byte[] rxBuf;

        private TraceLogger tectl;

        private bool CCDTemperatureSendCmdAction = true;

        public TECControl(string comPort, bool traceEnabled)
        {
            tecComPort = new SerialPort(comPort, baudrate, System.IO.Ports.Parity.None, 8, System.IO.Ports.StopBits.One);
            rxBuf = new byte[rxBufferSize];
            tectl = new TraceLogger("", "tec_cam8s_v09");
            tectl.Enabled = traceEnabled;
            tectl.LogMessage("TECControl", "Initialization finished");
        }

        public void Dispose()
        {
            tecComPort.Dispose();
            tectl.Dispose();
        }

        public bool Connect
        {
            get
            {
                tectl.LogMessage("Connect Get", "tecIsConnected=" + tecIsConnected.ToString());
                return tecIsConnected;
            }
            set
            {
                tectl.LogMessage("Connect Set", "value=" + value.ToString());
                if (value)
                {
                    try
                    {
                        tecComPort.Open();
                        tecComPort.DiscardInBuffer();
                        tecComPort.DiscardOutBuffer();
                        tecComPort.ReadTimeout = 500;
                        tecComPort.WriteTimeout = 500;
                        tectl.LogMessage("Connect Set", "COM open success");
                    }
                    catch
                    {
                        tecIsConnected = false;
                        tectl.LogMessage("Connect Set", "COM open fail");
                        return;
                    }
                    tecSendCommand(infoCmd);
                    Thread.Sleep(400);
                    if (tecReadPacket() == 1)
                    {
                        tecIsConnected = false;
                        tectl.LogMessage("Connect Set", "INFO packet received fail");
                        return;
                    }
                    tectl.LogMessage("Connect Set", "INFO packet received success");
                    tecSendCommand(getCmd);
                    Thread.Sleep(2000);
                    if (tecReadPacket() == 1)
                    {
                        tecIsConnected = false;
                        tectl.LogMessage("Connect Set", "first GET packet received fail");
                        return;
                    }
                    tectl.LogMessage("Connect Set", "first GET packet received success");
                    tecIsConnected = value;
                }
                else
                {
                    tecComPort.Close();
                    tectl.LogMessage("Connect Set", "COM close");
                    tecIsConnected = value;
                }
            }
        }

        public int TECError
        {
            get
            {
                tectl.LogMessage("TECError Get", "Error=" + tecErrorCode.ToString());
                return tecErrorCode;
            }
        }

        public double CCDTemperature
        {
            get
            {
                if (CCDTemperatureSendCmdAction)
                {
                    tectl.LogMessage("CCDTemperature Get", "tecSendCommand");
                    tecSendCommand(getCmd);
                    CCDTemperatureSendCmdAction = false;
                }
                else
                {
                    tectl.LogMessage("CCDTemperature Get", "tecReadPacket");
                    if (tecReadPacket() == 1) tecComPort.DiscardInBuffer();
                    CCDTemperatureSendCmdAction = true;
                }

                tectl.LogMessage("CCDTemperature Get", "CCDTemperature=" + tecCCDTemp.ToString());
                return tecCCDTemp;
            }
        }

        public double SetCCDTemperature
        {
            get
            {
                tectl.LogMessage("SetCCDTemperature Get", "SetCCDTemperature=" + tecSetTemp.ToString());
                return tecSetTemp;
            }
            set
            {
                tectl.LogMessage("SetCCDTemperature Set", "SetCCDTemperature=" + value.ToString());
                if ((value < this.MinSetTemperature) || (value > this.MaxSetTemperature))
                {
                    tectl.LogMessage("SetCCDTemperature Set", "InvalidValue, SetCCDTemperature must be in rang [minSetTemo;maxSetTemp");
                    return;
                }
                short tmp;
                tecSetTemp = value;
                tmp = (short)((value + tempOffset) * 10);
                setCmd[1] = (byte)((tmp >> 8) & 0x00ff);
                setCmd[2] = (byte)(tmp & 0x00ff);
                setCmd[3] = crc8_block(setCmd, 3);
                tecSendCommand(setCmd);
                Thread.Sleep(1000);
            }
        }

        public double HeatsinkTemperature
        {
            get
            {
                tectl.LogMessage("HeatsinkTemperature Get", "HeatsinkTemperature=" + tecHeatsinkTemp.ToString());
                return tecHeatsinkTemp;
            }
        }

        public double MaxSetTemperature
        {
            get
            {
                tectl.LogMessage("MaxSetTemperature Get", "MaxSetTemperature=" + maxSetTemp.ToString());
                return maxSetTemp;
            }
        }

        public double MinSetTemperature
        {
            get
            {
                tectl.LogMessage("MinSetTemperature Get", "MinSetTemperature=" + minSetTemp.ToString());
                return minSetTemp;
            }
        }

        public bool CoolerOn
        {
            get
            {
                tectl.LogMessage("CoolerOn Get", "CoolerOn=" + tecCoolenOn.ToString());
                return tecCoolenOn;
            }
            set
            {
                tecCoolenOn = value;
                if (value) pwmCmd[1] = 0x01;
                else pwmCmd[1] = 0x00;
                pwmCmd[2] = crc8_block(pwmCmd, 2);
                tecSendCommand(pwmCmd);
                tectl.LogMessage("CoolerOn Set", "CoolerOn=" + value.ToString());
                Thread.Sleep(1000);
            }
        }

        public double CoolerPower
        {
            get
            {
                tectl.LogMessage("CoolerPower Get", "CoolerPower=" + tecCoolerPower.ToString());
                return tecCoolerPower;
            }
        }

        private void tecSendCommand(byte[] cmd)
        {
            tecComPort.Write(cmd, 0, cmd.Length);
        }

        private byte crc8_block(byte[] pcBlock, byte len)
        {
            byte crc = 0xFF;
            byte i, j;
            for (j = 0; j < len; j++)
            {
                crc ^= pcBlock[j];
                for (i = 0; i < 8; i++)
                    if ((crc & 0x80) != 0) crc = (byte)((crc << 1) ^ 0x31);
                    else crc = (byte)(crc << 1);
            }
            return crc;
        }

        private byte tecReadPacket()
        {
            byte crc, i;
            for (i = 0; i < rxBufferSize; i++)
                rxBuf[i] = 0;
            if ((tecComPort.BytesToRead != responcePacketSize) && (tecComPort.BytesToRead != infoPacketSize)) return 1;
            tecComPort.Read(rxBuf, 0, tecComPort.BytesToRead);
            crc = crc8_block(rxBuf, (byte)(rxBuf.Length - 1));
            if (rxBuf[rxBuf.Length - 1] != crc) return 1;
            //check command responce v, d. renew interface if correct
            switch (rxBuf[0])
            {
                case 0x76:
                    {
                        if ((rxBuf[1] != hwRevision) || (rxBuf[2] != swRevision) || (rxBuf[3] != sensorCount) || (rxBuf[4] != setCount)) return 1;
                        else return 0;
                    };
                case 0x64:
                    {
                        tecCCDTemp = (((rxBuf[1] << 8) | (rxBuf[2])) - tempOffset * 10) / 10.0;
                        tecHeatsinkTemp = (((rxBuf[3] << 8) | (rxBuf[4])) - tempOffset * 10) / 10.0;
                        tecSetTemp = (((rxBuf[5] << 8) | (rxBuf[6])) - tempOffset * 10) / 10.0;
                        tecCoolerPower = (double)(rxBuf[7] * 100 / 255);
                        tecErrorCode = rxBuf[8];
                        if (rxBuf[9] == 0x00) tecCoolenOn = false;
                        else tecCoolenOn = true;
                        return 0;
                    };
                default:
                    {
                        return 1;
                    };
            }
        }
    }

    public class Camera : ICameraV2
    {
        /// <summary>
        /// ASCOM DeviceID (COM ProgID) for this driver.
        /// The DeviceID is used by ASCOM applications to load the driver at runtime.
        /// </summary>
        internal static string driverID = "ASCOM.cam8s_v09.Camera";

        /// <summary>
        /// Driver description that displays in the ASCOM Chooser.
        /// </summary>
        private static string driverDescription = "Cam8s v.0.9 ASCOM Driver";

        //parameters fro ASCOM profile read/write
        internal static string traceStateProfileName = "Trace Level";
        internal static string gainStateProfileName = "gain";
        internal static string offsetStateProfileName = "offset";
        internal static string onTopStateProfileName = "onTop";
        internal static string coolerEnabledStateProfileName = "coolerEnabled";
        internal static string coolerComPortStateProfileName = "coolerComPort";
        internal static string traceStateDefault = "false";
        internal static string gainStateDefault = "34";
        internal static string offsetStateDefault = "-7";
        internal static string onTopStateDefault = "true";
        internal static string coolerEnabledStateDefault = "false";
        internal static string coolerComPortStateDefault = "COM1";
        internal static bool traceState;
        internal static short gainState;
        internal static short offsetState;
        internal static bool onTopState;
        internal static bool coolerEnabledState;
        internal static string coolerComPortState;

        /// <summary>
        /// Form, handle gain/offset settings
        /// </summary>
        private camSettings settingsForm;

        private TECControl tec;

        /// <summary>
        /// Private variable to hold the connected state
        /// </summary>
        private bool cameraConnectedState;

        /// <summary>
        /// Private variable to hold an ASCOM Utilities object
        /// </summary>
        private Util utilities;

        /// <summary>
        /// Private variable to hold an ASCOM AstroUtilities object to provide the Range method
        /// </summary>
        private AstroUtils astroUtilities;

        /// <summary>
        /// Private variable to hold the trace logger object (creates a diagnostic log file with information that you specify)
        /// </summary>
        private TraceLogger tl;

        //Imports cam8sll09.dll functions
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraConnect();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraDisconnect();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraIsConnected();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraStartExposure(int Bin, int StartX, int StartY, int NumX, int NumY, double Duration, bool light);
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraStopExposure();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern int cameraGetCameraState();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraGetImageReady();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern uint cameraGetImage();
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraSetGain(int val);
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraSetOffset(int val);
        [DllImport("cam8sll09.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern int cameraGetError();

        /// <summary>
        /// Initializes a new instance of the <see cref="cam8s_v09"/> class.
        /// Must be public for COM registration.
        /// </summary>
        public Camera()
        {
            // Read device configuration from the ASCOM Profile store
            ReadProfile();
            //Init debug logger
            tl = new TraceLogger("", "cam8s_v09");
            tl.Enabled = traceState;
            tl.LogMessage("Camera", "Starting initialisation");
            // Initialise connected to false
            cameraConnectedState = false;
            //Initialise util object
            utilities = new Util();
            // Initialise astro utilities object
            astroUtilities = new AstroUtils();
            //New form for gain/offset settings
            settingsForm = new camSettings();
            settingsForm.gain = gainState;
            settingsForm.offset = offsetState;
            settingsForm.onTop = onTopState;
            if (!coolerEnabledState) settingsForm.tecStatus = "disabled";
            tec = new TECControl(coolerComPortState, traceState);
            tl.LogMessage("Camera", "Completed initialisation");
        }


        //
        // PUBLIC COM INTERFACE ICameraV2 IMPLEMENTATION
        //

        #region Common properties and methods.

        /// <summary>
        /// Displays the Setup Dialog form.
        /// If the user clicks the OK button to dismiss the form, then
        /// the new settings are saved, otherwise the old values are reloaded.
        /// THIS IS THE ONLY PLACE WHERE SHOWING USER INTERFACE IS ALLOWED!
        /// </summary>
        public void SetupDialog()
        {
            // show camera settings form
            if (IsConnected) return;
            using (SetupDialogForm F = new SetupDialogForm())
            {
                var result = F.ShowDialog();
                if (result == System.Windows.Forms.DialogResult.OK)
                {
                    // Persist device configuration values to the ASCOM Profile store
                    WriteProfile();
                }
            }
        }

        public ArrayList SupportedActions
        {
            get
            {
                tl.LogMessage("SupportedActions Get", "Returning empty arraylist");
                return new ArrayList();
            }
        }

        public string Action(string actionName, string actionParameters)
        {
            tl.LogMessage("Action", "Not implemented");
            throw new ASCOM.ActionNotImplementedException("Action " + actionName + " is not implemented by this driver");
        }

        public void CommandBlind(string command, bool raw)
        {
            tl.LogMessage("CommandBlind", "Not implemented");
            throw new ASCOM.MethodNotImplementedException("CommandBlind");
        }

        public bool CommandBool(string command, bool raw)
        {
            tl.LogMessage("CommandBool", "Not implemented");
            throw new ASCOM.MethodNotImplementedException("CommandBool");
        }

        public string CommandString(string command, bool raw)
        {
            tl.LogMessage("CommandString", "Not implemented");
            throw new ASCOM.MethodNotImplementedException("CommandString");
        }

        public void Dispose()
        {
            // Clean up the tracelogger, settings form and util objects
            tl.Enabled = false;
            tl.Dispose();
            tl = null;
            utilities.Dispose();
            utilities = null;
            astroUtilities.Dispose();
            astroUtilities = null;
            settingsForm.Dispose();
            tec.Dispose();
        }

        public bool Connected
        {
            get
            {
                tl.LogMessage("Connected Get", IsConnected.ToString());
                return IsConnected;
            }
            set
            {
                tl.LogMessage("Connected Set", value.ToString());
                if (value == IsConnected) return;
                if (value)
                {
                    tl.LogMessage("Connected Set", "Connecting to camera, call cameraConnect from cam8sll09.dll");
                    if (cameraConnect() == false)
                    {
                        tl.LogMessage("Connected Set", "Cant connect to cam8s");
                        throw new ASCOM.NotConnectedException("Cant connect to cam8s");
                    }
                    if (coolerEnabledState)
                    {
                        tl.LogMessage("Connected Set", "TEC Connect to module");
                        tec.Connect = true;
                        settingsForm.tecStatus = "connected";
                        if (tec.Connect == false)
                        {
                            tl.LogMessage("Connected Set", "TEC Connect to module failed");
                            System.Windows.Forms.MessageBox.Show("Cannot connect to selected TEC module, work without TEC module");
                            settingsForm.tecStatus = "disconnected";
                        }
                    }
                    tl.LogMessage("Connected Set", "cameraConnectedState=true");
                    cameraConnectedState = true;
                    settingsForm.Show();
                }
                else
                {
                    tl.LogMessage("Connected Set", "Disconnecting from camera, call cameraConnect from cam8sll09.dll");
                    if (cameraDisconnect() == false)
                    {
                        tl.LogMessage("Connected Set", "Cant disconnect cam8s");
                        throw new ASCOM.NotConnectedException("Cant disconnect cam8s");
                    }
                    if (coolerEnabledState)
                    {
                        tl.LogMessage("Connected Set", "TEC Disconnect from module");
                        tec.Connect = false;
                        settingsForm.tecStatus = "disconnected";
                    }
                    tl.LogMessage("Connected Set", "cameraConnectedState=false");
                    cameraConnectedState = false;
                    //save settings for ASCOM profile
                    WriteProfile();
                    settingsForm.Hide();
                }
            }
        }

        public string Description
        {
            get
            {
                tl.LogMessage("Description Get", driverDescription);
                return driverDescription;
            }
        }

        public string DriverInfo
        {
            get
            {
                tl.LogMessage("DriverInfo Get", driverDescription);
                return driverDescription;
            }
        }

        public string DriverVersion
        {
            get
            {
                Version version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version;
                string driverVersion = String.Format(CultureInfo.InvariantCulture, "{0}.{9}", version.Major, version.Minor);
                tl.LogMessage("DriverVersion Get", driverVersion);
                return driverVersion;
            }
        }

        public short InterfaceVersion
        {
            // set by the driver wizard
            get
            {
                tl.LogMessage("InterfaceVersion Get", "2");
                return Convert.ToInt16("2");
            }
        }

        public string Name
        {
            get
            {
                tl.LogMessage("Name Get", "cam8s");
                return "cam8s";
            }
        }

        #endregion

        #region ICamera Implementation

        // Constants to define the ccd pixel dimenstions
        private const int ccdWidth = 3000;
        private const int ccdHeight = 2000;
        // Constant for the pixel physical dimension um
        private const double pixelSize = 7.8;
        private const int maxPixelADU = 65535;

        private const int CameraStateIdle = 0;
        private const int CameraStateWaiting = 1;
        private const int CameraStateExposing = 2;
        private const int CameraStateReading = 3;
        private const int CameraStateDownloading = 4;
        private const int CameraSteteError = 5;

        private const int TecStatusConnected = 0;
        private const int TecStatusIntFailed = 1;
        private const int TecStatusExtFailed = 2;
        private const int TecStatusBothFailed = 3;
        private const int TecStatusUnknown = 4;

        // Initialise variables to hold values required for functionality tested by Conform
        private int cameraNumX = ccdWidth;
        private int cameraNumY = ccdHeight;
        private int cameraStartX = 0;
        private int cameraStartY = 0;
        private short cameraBinX = 1;
        private short cameraBinY = 1;
        private DateTime exposureStart = DateTime.MinValue;
        private double cameraLastExposureDuration = 0.0;

        private bool cameraImageReady = false;
        private int[,] cameraImageArray;

        private int cameraError = 0;

        public void AbortExposure()
        {
            tl.LogMessage("AbortExposure", "Aborting exposure, call cameraStopExposure from cam8sll09.dll");
            if (cameraStopExposure() == false)
            {
                tl.LogMessage("AbortExposure", "InvalidOperationException Abort Exposure failed");
                throw new ASCOM.InvalidOperationException("Abort Exposure failed");
            }
        }

        public short BayerOffsetX
        {
            get
            {
                tl.LogMessage("BayerOffsetX Get ", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("BayerOffsetX", true);
            }
        }

        public short BayerOffsetY
        {
            get
            {
                tl.LogMessage("BayerOffsetY Get ", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("BayerOffsetY", true);
            }
        }

        public short BinX
        {
            get
            {
                tl.LogMessage("BinX Get", cameraBinX.ToString());
                return cameraBinX;
            }
            set
            {
                tl.LogMessage("BinX Set", value.ToString());
                if ((value < 1) || (value > this.MaxBinX))
                {
                    tl.LogMessage("BinX Set", "InvalidValueException BinX must be in range [1;MaxBinX]");
                    throw new ASCOM.InvalidValueException("BinX", value.ToString(), "BinX must be in range [1;MaxBinX]");
                }
                cameraStartX = (cameraStartX * cameraBinX) / value;
                cameraNumX = (cameraNumX * cameraBinX) / value;
                cameraBinX = cameraBinY = value;
            }
        }

        public short BinY
        {
            get
            {
                tl.LogMessage("BinY Get", cameraBinY.ToString());
                return cameraBinY;
            }
            set
            {
                tl.LogMessage("BinY Set", value.ToString());
                if ((value < 1) || (value > this.MaxBinY))
                {
                    tl.LogMessage("BinY Set", "InvalidValueException BinY must be in range [1;MaxBinY]");
                    throw new ASCOM.InvalidValueException("BinY", value.ToString(), "BinY must be in range [1;MaxBinY]");
                }
                cameraStartY = (cameraStartY * cameraBinY) / value;
                cameraNumY = (cameraNumY * cameraBinY) / value;
                cameraBinY = cameraBinX = value;
            }
        }

        public double CCDTemperature
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CCDTemperature Get", "Check COM ports, if TEC COM not present - TEC module ejected");
                    string[] comPorts;
                    bool comPortEjected = true;
                    comPorts = SerialPort.GetPortNames();
                    int j;
                    for (j = 0; j < comPorts.Length; j++)
                        if (comPorts[j] == coolerComPortState) comPortEjected = false;
                    if (comPortEjected)
                    {
                        settingsForm.tecStatus = "ejected";
                        tec.Connect = false;
                        tl.LogMessage("CCDTemperature Get", coolerComPortState + "ejected, TEC module discennect");
                        throw new ASCOM.PropertyNotImplementedException("CCDTemperature", false);
                    }
                    switch (tec.TECError)
                    {
                        case TecStatusConnected:
                            {
                                settingsForm.tecStatus = "connected";
                                break;
                            }
                        case TecStatusIntFailed:
                            {
                                settingsForm.tecStatus = "internal sensor failed";
                                break;
                            }
                        case TecStatusExtFailed:
                            {
                                settingsForm.tecStatus = "external sensor failed";
                                break;
                            }
                        case TecStatusBothFailed:
                            {
                                settingsForm.tecStatus = "both sensors failed";
                                break;
                            }
                        default:
                            {
                                settingsForm.tecStatus = "unknown error";
                                break;
                            }
                    }
                    tl.LogMessage("CCDTemperature Get", "ccdTemp=" + tec.CCDTemperature.ToString());
                    return tec.CCDTemperature;
                }
                else
                {
                    tl.LogMessage("CCDTemperature Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("CCDTemperature", false);
                }
            }
        }

        public CameraStates CameraState
        {
            get
            {
                tl.LogMessage("CameraState Get", "Call cameraGetError from cam8sll09.dll");
                if (cameraError != (int)cameraGetError())
                {
                    settingsForm.cameraError = cameraError = (int)cameraGetError();
                    tl.LogMessage("CameraState Get", "cameraError = " + cameraError.ToString());
                }
                tl.LogMessage("CameraState Get", "Call cameraGetCameraState from cam8sll09.dll");
                switch ((short)cameraGetCameraState())
                {
                    case CameraStateIdle:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraIdle.ToString());
                            return CameraStates.cameraIdle;
                        }
                    case CameraStateWaiting:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraWaiting.ToString());
                            return CameraStates.cameraWaiting;
                        }
                    case CameraStateExposing:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraExposing.ToString());
                            return CameraStates.cameraExposing;
                        }
                    case CameraStateReading:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraReading.ToString());
                            return CameraStates.cameraReading;
                        }
                    case CameraStateDownloading:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraDownload.ToString());
                            return CameraStates.cameraDownload;
                        }
                    default:
                        {
                            tl.LogMessage("CameraState Get", CameraStates.cameraError.ToString());
                            return CameraStates.cameraError;
                        }
                }
            }
        }

        public int CameraXSize
        {
            get
            {
                tl.LogMessage("CameraXSize Get", ccdWidth.ToString());
                return ccdWidth;
            }
        }

        public int CameraYSize
        {
            get
            {
                tl.LogMessage("CameraYSize Get", ccdHeight.ToString());
                return ccdHeight;
            }
        }

        public bool CanAbortExposure
        {
            get
            {
                tl.LogMessage("CanAbortExposure Get", true.ToString());
                return true;
            }
        }

        public bool CanAsymmetricBin
        {
            get
            {
                tl.LogMessage("CanAsymmetricBin Get", false.ToString());
                return false;
            }
        }

        public bool CanFastReadout
        {
            get
            {
                tl.LogMessage("CanFastReadout Get", false.ToString());
                return false;
            }
        }

        public bool CanGetCoolerPower
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CanGetCoolerPower Get", "true");
                    return true;
                }
                else
                {
                    tl.LogMessage("CanGetCoolerPower Get", "false");
                    return false;
                }
            }
        }

        public bool CanPulseGuide
        {
            get
            {
                tl.LogMessage("CanPulseGuide Get", false.ToString());
                return false;
            }
        }

        public bool CanSetCCDTemperature
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CanSetCCDTemperature Get", "true");
                    return true;
                }
                else
                {
                    tl.LogMessage("CanSetCCDTemperature Get", "false");
                    return false;
                }
            }
        }

        public bool CanStopExposure
        {
            get
            {
                tl.LogMessage("CanStopExposure Get", true.ToString());
                return true;
            }
        }

        public bool CoolerOn
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CoolerOn Get", "coolerOn=" + tec.CoolerOn.ToString());
                    return tec.CoolerOn;
                }
                else
                {
                    tl.LogMessage("CoolerOn Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("CoolerOn", false);
                }
            }
            set
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CoolerOn Set", "coolerOn=" + value.ToString());
                    tec.CoolerOn = value;
                }
                else
                {
                    tl.LogMessage("CoolerOn Set Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("CoolerOn", true);
                }
            }
        }

        public double CoolerPower
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("CoolerPower Get", "coolerPower=" + tec.CoolerPower.ToString());
                    return tec.CoolerPower;
                }
                else
                {
                    tl.LogMessage("CoolerPower Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("CoolerPower", false);
                }
            }
        }

        public double ElectronsPerADU
        {
            get
            {
                tl.LogMessage("ElectronsPerADU Get ", "0.4");
                return 0.4;
            }
        }

        public double ExposureMax
        {
            get
            {
                tl.LogMessage("ExposureMax Get Get", "36000.00");
                return 36000.00;
            }
        }

        public double ExposureMin
        {
            get
            {
                tl.LogMessage("ExposureMin Get", "0.0");
                return 0.0;
            }
        }

        public double ExposureResolution
        {
            get
            {
                tl.LogMessage("ExposureResolution Get", "0.01");
                return 0.01;
            }
        }

        public bool FastReadout
        {
            get
            {
                tl.LogMessage("FastReadout Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("FastReadout", false);
            }
            set
            {
                tl.LogMessage("FastReadout Set", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("FastReadout", true);
            }
        }

        public double FullWellCapacity
        {
            get
            {
                tl.LogMessage("FullWellCapacity Get", "25000e");
                return 25000.0;
            }
        }

        public short Gain
        {
            get
            {
                tl.LogMessage("Gain Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("Gain", false);
            }
            set
            {
                tl.LogMessage("Gain Set", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("Gain", true);
            }
        }

        public short GainMax
        {
            get
            {
                tl.LogMessage("GainMax Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("GainMax", false);
            }
        }

        public short GainMin
        {
            get
            {
                tl.LogMessage("GainMin Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("GainMin", true);
            }
        }

        public ArrayList Gains
        {
            get
            {
                tl.LogMessage("Gains Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("Gains", true);
            }
        }

        public bool HasShutter
        {
            get
            {
                tl.LogMessage("HasShutter Get", true.ToString());
                return true;
            }
        }

        public double HeatSinkTemperature
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("HeatSinkTemperature Get", "heatsinkTemp=" + tec.HeatsinkTemperature.ToString());
                    return tec.HeatsinkTemperature;
                }
                else
                {
                    tl.LogMessage("HeatSinkTemperature Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("HeatSinkTemperature", false);
                }
            }
        }

        public object ImageArray
        {
            get
            {
                if (!cameraImageReady)
                {
                    tl.LogMessage("ImageArray Get", "Throwing InvalidOperationException because of a call to ImageArray before the first image has been taken!");
                    throw new ASCOM.InvalidOperationException("Call to ImageArray before the first image has been taken!");
                }

                uint imagepoint;
                //Get image pointer
                tl.LogMessage("ImageArray Get", "Call cameraGetImage from cam8sll09.dll");
                imagepoint = cameraGetImage();
                unsafe
                {
                    int* zeropixelpoint, pixelpoint;
                    //Set pixelpointers
                    zeropixelpoint = pixelpoint = (int*)imagepoint;
                    //Create image array
                    cameraImageArray = new int[cameraNumX, cameraNumY];
                    int i, j;
                    int ci, cj;

                    if (cameraBinX == 1)
                    {
                        cj = 0;
                        for (j = cameraStartY; j < (cameraStartY + cameraNumY); j++)
                        {
                            ci = 0;
                            for (i = cameraStartX; i < (cameraStartX + cameraNumX); i++)
                            {
                                pixelpoint = (int*)(zeropixelpoint + (i * ccdHeight + j));
                                cameraImageArray[ci, cj] = *pixelpoint;
                                if (cameraImageArray[ci, cj] > maxPixelADU) cameraImageArray[ci, cj] = maxPixelADU;
                                ci++;
                            }
                            cj++;
                        }
                    }
                    else
                    {
                        cj = 0;
                        for (j = cameraStartY; j < (cameraStartY + cameraNumY); j++)
                        {
                            ci = 0;
                            for (i = cameraStartX; i < (cameraStartX + cameraNumX); i++)
                            {
                                pixelpoint = (int*)(zeropixelpoint + (2 * i * ccdHeight + j * 2));
                                cameraImageArray[ci, cj] = *pixelpoint;
                                if (cameraImageArray[ci, cj] > maxPixelADU) cameraImageArray[ci, cj] = maxPixelADU;
                                ci++;
                            }
                            cj++;
                        }
                    }
                }
                return cameraImageArray;
            }
        }

        public object ImageArrayVariant
        {
            get
            {
                if (!cameraImageReady)
                {
                    tl.LogMessage("ImageArrayVariant Get", "Throwing InvalidOperationException because of a call to ImageArrayVariant before the first image has been taken!");
                    throw new ASCOM.InvalidOperationException("Call to ImageArrayVariant before the first image has been taken!");
                }
                tl.LogMessage("ImageArrayVariant Get", "Call ImageArray method");
                return this.ImageArray;
            }
        }

        public bool ImageReady
        {
            get
            {
                tl.LogMessage("ImageReady Get", "Call cameraGetImageReady from cam8sll09.dll");
                cameraImageReady = cameraGetImageReady();
                tl.LogMessage("ImageReady Get", cameraImageReady.ToString());
                return cameraImageReady;
            }
        }

        public bool IsPulseGuiding
        {
            get
            {
                tl.LogMessage("IsPulseGuiding Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("IsPulseGuiding", false);
            }
        }

        public double LastExposureDuration
        {
            get
            {
                if (!cameraImageReady)
                {
                    tl.LogMessage("LastExposureDuration Get", "Throwing InvalidOperationException because of a call to LastExposureDuration before the first image has been taken!");
                    throw new ASCOM.InvalidOperationException("Call to LastExposureDuration before the first image has been taken!");
                }
                tl.LogMessage("LastExposureDuration Get", cameraLastExposureDuration.ToString());
                return cameraLastExposureDuration;
            }
        }

        public string LastExposureStartTime
        {
            get
            {
                if (!cameraImageReady)
                {
                    tl.LogMessage("LastExposureStartTime Get", "Throwing InvalidOperationException because of a call to LastExposureStartTime before the first image has been taken!");
                    throw new ASCOM.InvalidOperationException("Call to LastExposureStartTime before the first image has been taken!");
                }
                string exposureStartString = exposureStart.ToString("yyyy-MM-ddTHH:mm:ss");
                tl.LogMessage("LastExposureStartTime Get", exposureStartString.ToString());
                return exposureStartString;
            }
        }

        public int MaxADU
        {
            get
            {
                tl.LogMessage("MaxADU Get", maxPixelADU.ToString());
                return maxPixelADU;
            }
        }

        public short MaxBinX
        {
            get
            {
                tl.LogMessage("MaxBinX Get", "2");
                return 2;
            }
        }

        public short MaxBinY
        {
            get
            {
                tl.LogMessage("MaxBinY Get", "2");
                return 2;
            }
        }

        public int NumX
        {
            get
            {
                tl.LogMessage("NumX Get", cameraNumX.ToString());
                return cameraNumX;
            }
            set
            {
                tl.LogMessage("NumX set", value.ToString());
                if ((value < 1) || (value > (ccdWidth / cameraBinX)))
                {
                    tl.LogMessage("NumX set", "InvalidValueException NumX must be in range [1;ccdWidth/cameraBinX]");
                    throw new InvalidValueException("NumX Set", value.ToString(), "NumX must be in range [1;ccdWidth/cameraBinX]");
                }
                cameraNumX = value;
            }
        }

        public int NumY
        {
            get
            {
                tl.LogMessage("NumY Get", cameraNumY.ToString());
                return cameraNumY;
            }
            set
            {
                tl.LogMessage("NumY set", value.ToString());
                if ((value < 1) || (value > (ccdHeight / cameraBinY)))
                {
                    tl.LogMessage("NumY set", "InvalidValueException NumY must be in range [1;ccdHeight/cameraBinY]");
                    throw new InvalidValueException("NumY Set", value.ToString(), "NumY must be in range [1;ccdHeight/cameraBinY]");
                }
                cameraNumY = value;
            }
        }

        public short PercentCompleted
        {
            get
            {
                tl.LogMessage("PercentCompleted Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("PercentCompleted", false);
            }
        }

        public double PixelSizeX
        {
            get
            {
                tl.LogMessage("PixelSizeX Get", pixelSize.ToString());
                return pixelSize;
            }
        }

        public double PixelSizeY
        {
            get
            {
                tl.LogMessage("PixelSizeY Get", pixelSize.ToString());
                return pixelSize;
            }
        }

        public void PulseGuide(GuideDirections Direction, int Duration)
        {
            tl.LogMessage("PulseGuide", "Not implemented");
            throw new ASCOM.MethodNotImplementedException("PulseGuide");
        }

        public short ReadoutMode
        {
            get
            {
                tl.LogMessage("ReadoutMode Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("ReadoutMode", false);
            }
            set
            {
                tl.LogMessage("ReadoutMode Set", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("ReadoutMode", true);
            }
        }

        public ArrayList ReadoutModes
        {
            get
            {
                tl.LogMessage("ReadoutModes Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("ReadoutModes", false);
            }
        }

        public string SensorName
        {
            get
            {
                tl.LogMessage("SensorName Get", "ICX453AQ");
                return "ICX453AQ";
            }
        }

        public SensorType SensorType
        {
            get
            {
                tl.LogMessage("SensorType Get", "SensorType.RGGB");
                return SensorType.RGGB;
            }
        }

        public double SetCCDTemperature
        {
            get
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("SetCCDTemperature Get", "heatsinkTemp=" + tec.SetCCDTemperature.ToString());
                    return tec.SetCCDTemperature;
                }
                else
                {
                    tl.LogMessage("SetCCDTemperature Get", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("SetCCDTemperature", false);
                }
            }
            set
            {
                if ((coolerEnabledState) && (tec.Connect))
                {
                    tl.LogMessage("SetCCDTemperature Set", "setCCDTemp=" + value.ToString());
                    if ((value < tec.MinSetTemperature) || (value > tec.MaxSetTemperature))
                    {
                        tl.LogMessage("SetCCDTemperature Set", "InvalidValueException SetCCDTemperature must be in range [minSetTemp;maxSetTemp]");
                        throw new InvalidValueException("SetCCDTemperature Set", value.ToString(), "SetCCDTemperature must be in range [-50;50]");
                    }
                    tec.SetCCDTemperature = value;
                }
                else
                {
                    tl.LogMessage("SetCCDTemperature Set", "Not implemented");
                    throw new ASCOM.PropertyNotImplementedException("SetCCDTemperature", true);
                }
            }
        }

        public void StartExposure(double Duration, bool Light)
        {
            //check exposure parameters
            tl.LogMessage("StartExposure", "Duration=" + Duration.ToString() + " Light=" + Light.ToString());
            if ((Duration < ExposureMin) || (Duration > ExposureMax))
            {
                tl.LogMessage("StartExposure", "InvalidValueException Duration must be in range [MinExposure;MaxExposure]");
                throw new InvalidValueException("StartExposure", Duration.ToString(), "Duration must be in range [MinExposure;MaxExposure]");
            }
            if ((cameraStartX + cameraNumX) > (ccdWidth / cameraBinX))
            {
                tl.LogMessage("StartExposure", "InvalidValueException (cameraStartX + cameraNumX) must be < (ccdWidth / cameraBinX)");
                throw new InvalidValueException("StartExposure", (cameraStartX + cameraNumX).ToString(), "(cameraStartX + cameraNumX) must be < (ccdWidth / cameraBinX)");
            }
            if ((cameraStartY + cameraNumY) > (ccdHeight / cameraBinY))
            {
                tl.LogMessage("StartExposure", "InvalidValueException (cameraStartY + cameraNumY) must be < (ccdHeight / cameraBinY)");
                throw new InvalidValueException("StartExposure", (cameraStartY + cameraNumY).ToString(), "(cameraStartY + cameraNumY) must be < (ccdHeight / cameraBinY)");
            }
            //set camera gain/offset
            tl.LogMessage("StartExposure", "Call cameraSetGain from cam8sll09.dll args: gain=" + settingsForm.gain.ToString());
            if (cameraSetGain(settingsForm.gain) == false)
            {
                tl.LogMessage("StartExposure", "Cant set gain to cam8s");
                throw new ASCOM.InvalidOperationException("Cant set gain to cam8s");
            }
            tl.LogMessage("StartExposure", "Call cameraSetOffset from cam8sll09.dll args: offset=" + settingsForm.offset.ToString());
            if (cameraSetOffset(settingsForm.offset) == false)
            {
                tl.LogMessage("StartExposure", "Cant set offset to cam8s");
                throw new ASCOM.InvalidOperationException("Cant set offset to cam8s");
            }
            //Save parameters
            cameraLastExposureDuration = Duration;
            exposureStart = DateTime.Now;
            gainState = settingsForm.gain;
            offsetState = settingsForm.offset;
            onTopState = settingsForm.onTop;
            //start exposure
            tl.LogMessage("StartExposure", "Call cameraStartExposure from cam8sll09.dll, args: ");
            tl.LogMessage("StartExposure", " cameraBinX=" + cameraBinX.ToString() +
                                           " cameraStartX=" + cameraStartX.ToString() +
                                           " cameraStartY=" + cameraStartY.ToString() +
                                           " cameraNumX=" + cameraNumX.ToString() +
                                           " cameraNumY=" + cameraNumY.ToString() +
                                           " Duration=" + Duration.ToString() +
                                           " Light=" + true.ToString());
            cameraStartExposure((int)cameraBinX, cameraStartX * cameraBinX, cameraStartY * cameraBinY, cameraNumX * cameraBinX, cameraNumY * cameraBinY, Duration, Light);
        }

        public int StartX
        {
            get
            {
                tl.LogMessage("StartX Get", cameraStartX.ToString());
                return cameraStartX;
            }
            set
            {
                tl.LogMessage("StartX Set", value.ToString());
                if ((value < 0) || (value >= (ccdWidth / cameraBinX)))
                {
                    tl.LogMessage("StartX Set", "InvalidValueException StartX must be in range [0;ccdWidth/cameraBinX)");
                    throw new InvalidValueException("StartX Set", value.ToString(), "StartX must be in range [0;ccdWidth/cameraBinX)");
                }
                cameraStartX = value;
            }
        }

        public int StartY
        {
            get
            {
                tl.LogMessage("StartY Get", cameraStartY.ToString());
                return cameraStartY;
            }
            set
            {
                tl.LogMessage("StartY set", value.ToString());
                if ((value < 0) || (value >= (ccdHeight / cameraBinY)))
                {
                    tl.LogMessage("StartY Set", "InvalidValueException StartY must be in range [0;ccdHeight/cameraBinY)");
                    throw new InvalidValueException("StartY Set", value.ToString(), "StartY must be in range [0;ccdHeight/cameraBinY)");
                }
                cameraStartY = value;
            }
        }

        public void StopExposure()
        {
            tl.LogMessage("StopExposure", "Aborting exposure, call cameraStopExposure from cam8sll09.dll");
            if (cameraStopExposure() == false)
            {
                tl.LogMessage("StopExposure", "InvalidOperationException Stop Exposure failed");
                throw new ASCOM.InvalidOperationException("Stop Exposure failed");
            }
        }

        #endregion

        #region Private properties and methods
        // here are some useful properties and methods that can be used as required
        // to help with driver development

        #region ASCOM Registration

        // Register or unregister driver for ASCOM. This is harmless if already
        // registered or unregistered. 
        //
        /// <summary>
        /// Register or unregister the driver with the ASCOM Platform.
        /// This is harmless if the driver is already registered/unregistered.
        /// </summary>
        /// <param name="bRegister">If <c>true</c>, registers the driver, otherwise unregisters it.</param>
        private static void RegUnregASCOM(bool bRegister)
        {
            using (var P = new ASCOM.Utilities.Profile())
            {
                P.DeviceType = "Camera";
                if (bRegister)
                {
                    P.Register(driverID, driverDescription);
                }
                else
                {
                    P.Unregister(driverID);
                }
            }
        }

        /// <summary>
        /// This function registers the driver with the ASCOM Chooser and
        /// is called automatically whenever this class is registered for COM Interop.
        /// </summary>
        /// <param name="t">Type of the class being registered, not used.</param>
        /// <remarks>
        /// This method typically runs in two distinct situations:
        /// <list type="numbered">
        /// <item>
        /// In Visual Studio, when the project is successfully built.
        /// For this to work correctly, the option <c>Register for COM Interop</c>
        /// must be enabled in the project settings.
        /// </item>
        /// <item>During setup, when the installer registers the assembly for COM Interop.</item>
        /// </list>
        /// This technique should mean that it is never necessary to manually register a driver with ASCOM.
        /// </remarks>
        [ComRegisterFunction]
        public static void RegisterASCOM(Type t)
        {
            RegUnregASCOM(true);
        }

        /// <summary>
        /// This function unregisters the driver from the ASCOM Chooser and
        /// is called automatically whenever this class is unregistered from COM Interop.
        /// </summary>
        /// <param name="t">Type of the class being registered, not used.</param>
        /// <remarks>
        /// This method typically runs in two distinct situations:
        /// <list type="numbered">
        /// <item>
        /// In Visual Studio, when the project is cleaned or prior to rebuilding.
        /// For this to work correctly, the option <c>Register for COM Interop</c>
        /// must be enabled in the project settings.
        /// </item>
        /// <item>During uninstall, when the installer unregisters the assembly from COM Interop.</item>
        /// </list>
        /// This technique should mean that it is never necessary to manually unregister a driver from ASCOM.
        /// </remarks>
        [ComUnregisterFunction]
        public static void UnregisterASCOM(Type t)
        {
            RegUnregASCOM(false);
        }

        #endregion

        /// <summary>
        /// Returns true if there is a valid connection to the driver hardware
        /// </summary>
        private bool IsConnected
        {
            get
            {
                tl.LogMessage("IsConnected Get", "Call cameraIsConnected from cam8sll09.dll");
                cameraConnectedState = cameraIsConnected();
                tl.LogMessage("IsConnected Get", "connectedState=" + cameraConnectedState.ToString());
                return cameraConnectedState;
            }
        }

        /// <summary>
        /// Use this function to throw an exception if we aren't connected to the hardware
        /// </summary>
        /// <param name="message"></param>
        private void CheckConnected(string message)
        {
            if (!IsConnected)
            {
                tl.LogMessage("CheckConnected", "connectedState=false" + message);
                throw new ASCOM.NotConnectedException(message);
            }
        }

        /// <summary>
        /// Read the device configuration from the ASCOM Profile store
        /// </summary>
        internal void ReadProfile()
        {
            using (Profile driverProfile = new Profile())
            {
                driverProfile.DeviceType = "Camera";
                traceState = Convert.ToBoolean(driverProfile.GetValue(driverID, traceStateProfileName, string.Empty, traceStateDefault));
                gainState = Convert.ToInt16(driverProfile.GetValue(driverID, gainStateProfileName, string.Empty, gainStateDefault));
                offsetState = Convert.ToInt16(driverProfile.GetValue(driverID, offsetStateProfileName, string.Empty, offsetStateDefault));
                onTopState = Convert.ToBoolean(driverProfile.GetValue(driverID, onTopStateProfileName, string.Empty, onTopStateDefault));
                coolerEnabledState = Convert.ToBoolean(driverProfile.GetValue(driverID, coolerEnabledStateProfileName, string.Empty, coolerEnabledStateDefault));
                coolerComPortState = driverProfile.GetValue(driverID, coolerComPortStateProfileName, string.Empty, coolerComPortStateDefault);
            }
        }

        /// <summary>
        /// Write the device configuration to the  ASCOM  Profile store
        /// </summary>
        internal void WriteProfile()
        {
            using (Profile driverProfile = new Profile())
            {
                driverProfile.DeviceType = "Camera";
                driverProfile.WriteValue(driverID, traceStateProfileName, traceState.ToString());
                driverProfile.WriteValue(driverID, gainStateProfileName, gainState.ToString());
                driverProfile.WriteValue(driverID, offsetStateProfileName, offsetState.ToString());
                driverProfile.WriteValue(driverID, onTopStateProfileName, onTopState.ToString());
                driverProfile.WriteValue(driverID, coolerEnabledStateProfileName, coolerEnabledState.ToString());
                driverProfile.WriteValue(driverID, coolerComPortStateProfileName, coolerComPortState.ToString());
            }
        }

        #endregion

    }
}
