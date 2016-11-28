// --------------------------------------------------------------------------------
// ASCOM Camera driver for cam86 v.0.1
// Edit Log:
// Date			Who	Vers	Description
// -----------	---	-----	-------------------------------------------------------
// 29-aug-2016  VSS 0.1     Initial
// --------------------------------------------------------------------------------

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
using System.Threading;
using System.Timers;

namespace ASCOM.cam86
{
    /// <summary>
    /// ASCOM Camera Driver for cam86.
    /// </summary>
    [Guid("677df06a-d784-4a3b-9028-d597e58131a4")]
    [ClassInterface(ClassInterfaceType.None)]
    public class Camera : ICameraV2
    {
        /// <summary>
        /// ASCOM DeviceID (COM ProgID) for this driver.
        /// The DeviceID is used by ASCOM applications to load the driver at runtime.
        /// </summary>
        internal static string driverID = "ASCOM.cam86.Camera";

        /// <summary>
        /// Driver description that displays in the ASCOM Chooser.
        /// </summary>
        private static string driverDescription = "Cam86 v.0.1 ASCOM Driver";

        //parameters fro ASCOM profile read/write
        internal static string traceStateProfileName = "Trace Level";
        internal static string gainStateProfileName = "gain";
        internal static string offsetStateProfileName = "offset";
        internal static string onTopStateProfileName = "onTop";
        internal static string slowCoolingEnabledProfileName = "slowCooling";
        internal static string slowCoolingSpeedProfileName = "slowCoolingSpeed";
        internal static string traceStateDefault = "false";
        internal static string gainStateDefault = "0";
        internal static string offsetStateDefault = "-6";
        internal static string onTopStateDefault = "true";
        internal static string slowCoolingEnabledStateDefault = "false";
        internal static string slowCoolingSpeedStateDefault = "5";
        internal static bool traceState;
        internal static short gainState;
        internal static short offsetState;
        internal static bool onTopState;
        internal static bool slowCoolingEnabledState;
        internal static short slowCoolingSpeedState;

        /// <summary>
        /// Form, handle gain/offset settings
        /// </summary>
        private static camSettings settingsForm;

        /// <summary>
        /// Private variable to hold the connected state
        /// </summary>
        private bool cameraConnectedState;

        /// <summary>
        /// Private variable to hold an ASCOM Utilities object
        /// </summary>
        private Util utilities;

        /// <summary>
        /// Private variable to hold the trace logger object (creates a diagnostic log file with information that you specify)
        /// </summary>
        private static TraceLogger tl;

        private const string LowLevelDLL = "cam86ll.dll";

        //Imports cam86ll.dll functions
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraConnect();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraDisconnect();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraIsConnected();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraStartExposure(int Bin, int StartX, int StartY, int NumX, int NumY, double Duration, bool light);
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraStopExposure();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern int cameraGetCameraState();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraGetImageReady();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern uint cameraGetImage();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraSetGain(int val);
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraSetOffset(int val);
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern int cameraGetError();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern double cameraGetTemp();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraSetTemp(double temp);
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern double cameraGetSetTemp();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraCoolingOn();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraCoolingOff();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool cameraGetCoolerOn();
        [DllImport(LowLevelDLL, CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern double cameraGetCoolerPower();


        /// <summary>
        /// Initializes a new instance of the <see cref="cam86"/> class.
        /// Must be public for COM registration.
        /// </summary>
        public Camera()
        {
            // Read device configuration from the ASCOM Profile store
            ReadProfile();
            //Init debug logger
            tl = new TraceLogger("", "cam86");
            tl.Enabled = traceState;
            tl.LogMessage("Camera", "Starting initialisation");
            // Initialise connected to false
            cameraConnectedState = false;
            //Initialise util object
            utilities = new Util();
            //New form for gain/offset settings
            settingsForm = new camSettings();
            settingsForm.gain = gainState;
            settingsForm.offset = offsetState;
            settingsForm.onTop = onTopState;
            settingsForm.slowCoolingEnabled = slowCoolingEnabledState;
            settingsForm.slowCoolingSpeed = slowCoolingSpeedState;
            slowCoolingTimer = new System.Timers.Timer(60000);
            slowCoolingTimer.Enabled = false;
            slowCoolingTimer.Elapsed += slowCoolingTimerTick;
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
            settingsForm.Dispose();
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
                    tl.LogMessage("Connected Set", "Connecting to camera, call cameraConnect from " + LowLevelDLL);
                    if (cameraConnect() == false)
                    {
                        tl.LogMessage("Connected Set", "Cant connect to " + this.Name);
                        throw new ASCOM.NotConnectedException("Cant connect to " + this.Name);
                    }
                    tl.LogMessage("Connected Set", "cameraConnectedState=true");
                    cameraConnectedState = true;
                    settingsForm.Show();
                }
                else
                {
                    tl.LogMessage("Connected Set", "Disconnecting from camera, call cameraConnect from" + LowLevelDLL);
                    if (cameraDisconnect() == false)
                    {
                        tl.LogMessage("Connected Set", "Cant disconnect " + this.Name);
                        throw new ASCOM.NotConnectedException("Cant disconnect " + this.Name);
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
                string driverVersion = String.Format(CultureInfo.InvariantCulture, "{0}.{1}", version.Major, version.Minor);
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
                tl.LogMessage("Name Get", "cam86");
                return "cam86";
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

        private const double MinSetTemp = -50.0;
        private const double MaxSetTemp = 50.0;

        private static double slowCoolingInterm = 0.0;
        private static bool slowCoolingCoolingDirection = true; //true==cooling, false==heating
        private static double slowCoolingTarger = 0.0;
        private static System.Timers.Timer slowCoolingTimer;


        public void AbortExposure()
        {
            tl.LogMessage("AbortExposure", "Aborting exposure, call cameraStopExposure from " + LowLevelDLL);
            if (cameraStopExposure() == false)
            {
                tl.LogMessage("AbortExposure", "PropertyNotImplementedException Abort Exposure failed");
                throw new ASCOM.PropertyNotImplementedException("Abort Exposure failed");
            }
        }

        public short BayerOffsetX
        {
            get
            {
                tl.LogMessage("BayerOffsetX Get ", "1");
                return 1;
            }
        }

        public short BayerOffsetY
        {
            get
            {
                tl.LogMessage("BayerOffsetY Get ", "0");
                return 0;
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
                double temp = cameraGetTemp();
                tl.LogMessage("CCDTemperature Get", temp.ToString());
                return temp;
            }
        }

        public CameraStates CameraState
        {
            get
            {
                tl.LogMessage("CameraState Get", "Call cameraGetError from " + LowLevelDLL);
                if (cameraError != (int)cameraGetError())
                {
                    settingsForm.cameraError = cameraError = (int)cameraGetError();
                    tl.LogMessage("CameraState Get", "cameraError = " + cameraError.ToString());
                }
                tl.LogMessage("CameraState Get", "Call cameraGetCameraState from " + LowLevelDLL);
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
                tl.LogMessage("CanGetCoolerPower Get", true.ToString());
                return true;
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
                tl.LogMessage("CanSetCCDTemperature Get", "true");
                return true;
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
                bool coolerState = cameraGetCoolerOn();
                tl.LogMessage("CoolerOn Get", coolerState.ToString());
                return coolerState;
            }
            set
            {
                tl.LogMessage("CoolerOn Set", value.ToString());
                if (value)
                {
                    this.SetCCDTemperature = this.SetCCDTemperature;
                    cameraCoolingOn();
                }
                else
                {
                    slowCoolingTimer.Enabled = false;
                    cameraCoolingOff();
                }
            }
        }

        public double CoolerPower
        {
            get
            {
                double coolerPower = cameraGetCoolerPower();
                tl.LogMessage("CoolerPower Get", coolerPower.ToString());
                return coolerPower;
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
                tl.LogMessage("ExposureMax Get", "36000.00");
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
                tl.LogMessage("HasShutter Get", false.ToString());
                return false;
            }
        }

        public double HeatSinkTemperature
        {
            get
            {
                tl.LogMessage("HeatSinkTemperature Get", "Not implemented");
                throw new ASCOM.PropertyNotImplementedException("HeatSinkTemperature", false);
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
                tl.LogMessage("ImageArray Get", "Call cameraGetImage from " + LowLevelDLL);
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
                                pixelpoint = (int*)(zeropixelpoint + (j * ccdWidth + i));
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
                                pixelpoint = (int*)(zeropixelpoint + (2 * j * ccdWidth + i * 2));
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
                tl.LogMessage("ImageReady Get", "Call cameraGetImageReady from " + LowLevelDLL);
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
                double temp = cameraGetSetTemp();
                tl.LogMessage("SetCCDTemperature Get", temp.ToString());
                return temp;
            }
            set
            {
                tl.LogMessage("SetCCDTemperature Set", value.ToString());

                if ((value < MinSetTemp) || (value > MaxSetTemp))
                {
                    tl.LogMessage("SetCCDTemperature Set", "InvalidValueException SetCCDTemperature must be in range [minSetTemp;maxSetTemp]");
                    throw new InvalidValueException("SetCCDTemperature Set", value.ToString(), "SetCCDTemperature must be in range [-50;50]");
                }

                if ((settingsForm.slowCoolingEnabled) && (this.CoolerOn))
                {
                    tl.LogMessage("SetCCDTemperature Set", "start slow cooling with step size=" + (settingsForm.slowCoolingSpeed / 10.0).ToString());
                    slowCoolingTarger = value;
                    slowCoolingInterm = this.CCDTemperature;
                    if ((this.CCDTemperature - value) > 0)
                    {
                        slowCoolingCoolingDirection = true;
                        slowCoolingTimer.Enabled = true;
                    }
                    else if ((this.CCDTemperature - value) < 0)
                    {
                        slowCoolingCoolingDirection = false;
                        slowCoolingTimer.Enabled = true;
                    }
                    else
                    {
                        slowCoolingTimer.Enabled = false;
                        cameraSetTemp(value);
                    }
                }
                else
                {
                    cameraSetTemp(value);
                }
            }
        }

        private static void slowCoolingTimerTick(Object source, ElapsedEventArgs e)
        {
            if (slowCoolingTimer.Enabled && settingsForm.slowCoolingEnabled)
            {
                tl.LogMessage("slowCoolingTimerTick", "slowCoolingInterm=" + slowCoolingInterm.ToString() + " step size=" + (settingsForm.slowCoolingSpeed / 10.0).ToString());
                if (slowCoolingCoolingDirection)
                {
                    slowCoolingInterm -= (settingsForm.slowCoolingSpeed / 10.0);
                    if (slowCoolingInterm <= slowCoolingTarger)
                    {
                        slowCoolingInterm = slowCoolingTarger;
                    }
                }
                else
                {
                    slowCoolingInterm += (settingsForm.slowCoolingSpeed / 10.0);
                    if (slowCoolingInterm >= slowCoolingTarger)
                    {
                        slowCoolingInterm = slowCoolingTarger;
                    }
                }
                cameraSetTemp(slowCoolingInterm);
                if (System.Math.Abs(slowCoolingInterm - slowCoolingTarger) <= 0.01)
                {
                    slowCoolingTimer.Enabled = false;
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
            tl.LogMessage("StartExposure", "Call cameraSetGain from " + LowLevelDLL + " args: gain=" + settingsForm.gain.ToString());
            if (cameraSetGain(settingsForm.gain) == false)
            {
                tl.LogMessage("StartExposure", "Cant set gain to " + this.Name);
                throw new ASCOM.InvalidOperationException("Cant set gain to " + this.Name);
            }
            tl.LogMessage("StartExposure", "Call cameraSetOffset from " + LowLevelDLL + " args: offset=" + settingsForm.offset.ToString());
            if (cameraSetOffset(settingsForm.offset) == false)
            {
                tl.LogMessage("StartExposure", "Cant set offset to " + this.Name);
                throw new ASCOM.InvalidOperationException("Cant set offset to " + this.Name);
            }
            //settle delay
            Thread.Sleep(100);
            //Save parameters
            cameraLastExposureDuration = Duration;
            exposureStart = DateTime.Now;
            gainState = settingsForm.gain;
            offsetState = settingsForm.offset;
            onTopState = settingsForm.onTop;
            slowCoolingEnabledState = settingsForm.slowCoolingEnabled;
            slowCoolingSpeedState = settingsForm.slowCoolingSpeed;
            //start exposure
            tl.LogMessage("StartExposure", "Call cameraStartExposure from " + LowLevelDLL + " args: ");
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
            tl.LogMessage("StopExposure", "Aborting exposure, call cameraStopExposure from " + LowLevelDLL);
            if (cameraStopExposure() == false)
            {
                tl.LogMessage("StopExposure", "PropertyNotImplementedException Stop Exposure failed");
                throw new ASCOM.PropertyNotImplementedException("Stop Exposure failed");
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
                tl.LogMessage("IsConnected Get", "Call cameraIsConnected from " + LowLevelDLL);
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
                slowCoolingEnabledState = Convert.ToBoolean(driverProfile.GetValue(driverID, slowCoolingEnabledProfileName, string.Empty, slowCoolingEnabledStateDefault));
                slowCoolingSpeedState = Convert.ToInt16(driverProfile.GetValue(driverID, slowCoolingSpeedProfileName, string.Empty, slowCoolingSpeedStateDefault));
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
                driverProfile.WriteValue(driverID, slowCoolingEnabledProfileName, slowCoolingEnabledState.ToString());
                driverProfile.WriteValue(driverID, slowCoolingSpeedProfileName, slowCoolingSpeedState.ToString());
            }
        }

        #endregion

    }
}
