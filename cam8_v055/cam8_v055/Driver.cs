// --------------------------------------------------------------------------------
// ASCOM Camera driver for cam8 v.0.55
// Edit Log:
// Date			Who	Vers	Description
// -----------	---	-----	-------------------------------------------------------
// 30-dec-2012	VSS	0.1 	Initial edit, created from ASCOM driver template
// 20-jan-2013  VSS 0.2     Add functional implemented in version 0.2
// 12-mar-2013  VSS 0.3     Fixed horizontal line in top of frame, implement hardware ROI, faster image reading prodecure
// 19-may-2013  VSS 0.4     Improved image read performance and user interface
// 24-sep-2014  VSS 0.5     Fixed bug with small exposures, saving gain/offset settings
// 10-oct-2014  VSS 0.51    Fixed minor bug in readframe function in cam8ll051.dll
// 27-nov-2014  VSS 0.55    Fixed minor bugs with saving gain/offset settings
// --------------------------------------------------------------------------------
#define Camera

using System;
using System.Collections.Generic;
using System.Text;
using System.Timers;
using System.Threading;
using System.Runtime.InteropServices;

using ASCOM;
using ASCOM.Utilities;
using ASCOM.DeviceInterface;
using System.Globalization;
using System.Collections;
//.NET adapted C++ FTD2XX library
using FTD2XX_NET;
using System.IO; // work with files
using System.Xml.Serialization; // class savings

namespace ASCOM.cam8_v055
{
    [Guid("941027cb-0cdf-4e21-9fc0-20d1920a33ee")]
    [ClassInterface(ClassInterfaceType.None)]

    public class iniSettings
    {
        public short gain;
        public short offset;

        public iniSettings()
        {
            gain = 0;
            offset = 0;
        }
    }

    public class Camera : ICameraV2
    {
        //Const, camera size
        const int CameraXSizeConst = 3000;
        const int CameraYSizeConst = 2000;

        public static string driverID = "ASCOM.cam8_v055.Camera";
        private static string driverDescription = "Cam8 v.0.55 ASCOM Driver";
        private string SettingFilePath = "";
        //Common camera property variables
        private short m_BinX;
        private short m_BinY;
        private short m_CameraState;
        private ArrayList m_Gains;
        private bool m_ImageReady;
        private Array m_Image;
        private int m_NumX;
        private int m_NumY;
        private int m_StartX;
        private int m_StartY;
        private double m_LastExposureDuration;
        private String m_LastExposureStartTime;
        private short m_ReadoutMode;
        private ArrayList m_ReadoutModes;
        //Special camera properties for CAM8
        private short m_Gain;
        private short m_Offset;

        private cam_settings settings_form;
#if Telescope
        //
        // Driver private data (rate collections) for the telescope driver only.
        // This can be removed for other driver types
        //
        private readonly AxisRates[] _axisRates;
#endif
        /// <summary>
        /// Initializes a new instance of the <see cref="cam81_v055"/> class.
        /// Must be public for COM registration.
        /// </summary>
        /// 

        //Imports cam8ll055.dll functions
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraConnect();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraIsConnected();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraDisconnect();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraStartExposure(int Bin, int StartX, int StartY, int NumX, int NumY, double Duration, bool light);
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraStopExposure();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern int CameraGetCameraState();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraGetImageReady();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern uint CameraGetImage();
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraSetGain(int val);
        [DllImport("cam8ll055.dll", CallingConvention = CallingConvention.StdCall, CharSet = CharSet.Ansi)]
        static extern bool CameraSetOffset(int val);
        //Camera constructor
        public Camera()
        {
#if Telescope
            // the rates constructors are only needed for the telescope class
            // This can be removed for other driver types
            _axisRates = new AxisRates[3];
            _axisRates[0] = new AxisRates(TelescopeAxes.axisPrimary);
            _axisRates[1] = new AxisRates(TelescopeAxes.axisSecondary);
            _axisRates[2] = new AxisRates(TelescopeAxes.axisTertiary);
#endif
            //Camera initial state

            string arch = System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE").ToString();
            if (arch.IndexOf("86") != -1) SettingFilePath = Environment.ExpandEnvironmentVariables("%CommonProgramFiles%\\ASCOM\\Camera\\cam8\\cam8_v05.xml");
            else SettingFilePath = Environment.ExpandEnvironmentVariables("%CommonProgramFiles(x86)%\\ASCOM\\Camera\\cam8\\cam8_v05.xml");

            m_BinX = 1;
            m_BinY = 1;
            m_CameraState = 0;
            m_ImageReady = false;
            m_NumX = CameraXSizeConst;
            m_NumY = CameraYSizeConst;
            m_StartX = 0;
            m_StartY = 0;
            m_LastExposureDuration = ExposureMin;
            m_LastExposureStartTime = "2012-12-21T12:00:00";
            //Camera gains initial state
            m_Gains = new ArrayList();
            m_Gains.Clear();
            short i;
            for (i = 0; i < 64; i++)
                m_Gains.Add(i.ToString());
            //Readout modes initial state
            m_ReadoutMode = 0;
            m_ReadoutModes = new ArrayList();
            m_ReadoutModes.Clear();
            m_ReadoutModes.Add("standart");
            //Special camera properties initials values
            m_Gain = 34;
            m_Offset = -7;

            settings_form = new cam_settings();
        }

        #region ASCOM Registration
        //
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

        //
        // PUBLIC COM INTERFACE ICameraV2 IMPLEMENTATION
        //

        /// <summary>
        /// Displays the Setup Dialog form.
        /// If the user clicks the OK button to dismiss the form, then
        /// the new settings are saved, otherwise the old values are reloaded.
        /// THIS IS THE ONLY PLACE WHERE SHOWING USER INTERFACE IS ALLOWED!
        /// </summary>
        /// 

        public void SetupDialog()
        {
            // consider only showing the setup dialog if not connected
            // or call a different dialog if connected                
            if (IsConnected) System.Windows.Forms.MessageBox.Show("Already connected, just press OK");

            using (SetupDialogForm F = new SetupDialogForm())
            {
                var result = F.ShowDialog();
                if (result == System.Windows.Forms.DialogResult.OK)
                {
                    //Properties.Settings.Default.Save();
                    return;
                }
                //Properties.Settings.Default.Reload();
            }
        }

        #region common properties and methods. All set to no action

        public System.Collections.ArrayList SupportedActions
        {
            get { return new ArrayList(); }
        }

        public string Action(string actionName, string actionParameters)
        {
            throw new ASCOM.MethodNotImplementedException("Action");
        }

        public void CommandBlind(string command, bool raw)
        {
            CheckConnected("CommandBlind");
            // Call CommandString and return as soon as it finishes
            this.CommandString(command, raw);
            // or
            throw new ASCOM.MethodNotImplementedException("CommandBlind");
        }

        public bool CommandBool(string command, bool raw)
        {
            CheckConnected("CommandBool");
            string ret = CommandString(command, raw);
            // TODO decode the return string and return true or false
            // or
            throw new ASCOM.MethodNotImplementedException("CommandBool");
        }

        public string CommandString(string command, bool raw)
        {
            CheckConnected("CommandString");
            // it's a good idea to put all the low level communication with the device here,
            // then all communication calls this function
            // you need something to ensure that only one command is in progress at a time
            throw new ASCOM.MethodNotImplementedException("CommandString");
        }
        //Aborts camera exposure
        public void AbortExposure()
        {
            if (CameraStopExposure() == false) throw new ASCOM.PropertyNotImplementedException("Abort Exposure failed");
        }
        //Camera Pulse Guide, doesn't supported in hardware
        public void PulseGuide(GuideDirections Direction, int Duretion)
        {
            throw new ASCOM.PropertyNotImplementedException("PulseGuide not supported now");
        }
        //Camera start exposure
        public void StartExposure(double Duration, bool Light)
        {
            if ((Duration < ExposureMin) || (Duration > ExposureMax)) throw new ASCOM.InvalidValueException("StartExposure: Invalid Duration");
            //Set LastExposureDuration;
            m_LastExposureDuration = Duration;
            //Set LasExposureStartTime;
            m_LastExposureStartTime = System.DateTime.Now.ToString("yyyy-MM-ddThh:mm:ss");
            //Read & set gain and offset parameters
            m_Gain = short.Parse(settings_form.GainTextBox.Text);
            m_Offset = short.Parse(settings_form.OffsetTextBox.Text);
            //Set camera settings, if fail - exception
            if (CameraSetGain(m_Gain) == false) throw new ASCOM.InvalidOperationException("Cant set gain to cam8");
            if (CameraSetOffset(m_Offset) == false) throw new ASCOM.InvalidOperationException("Cant set offset to cam8");

            //save gain, offset settings
            iniSettings iniSet = new iniSettings();
            iniSet.gain = m_Gain;
            iniSet.offset = m_Offset;
            using (Stream writer = new FileStream(SettingFilePath, FileMode.Create))
            {
                XmlSerializer serializer = new XmlSerializer(typeof(iniSettings));
                serializer.Serialize(writer, iniSet);
            }
            //Call function from cam8ll55.dll to start exposure
            CameraStartExposure((int)m_BinX, m_StartX, m_StartY, m_NumX, m_NumY, Duration, true);
            return;
        }
        //Camera stop exposure
        public void StopExposure()
        {
            if (CameraStopExposure() == false) throw new ASCOM.PropertyNotImplementedException("Stop Exposure failed");
        }

        #endregion

        #region public properties and methods

        public void Dispose()
        {
            throw new System.NotImplementedException();
        }

        public bool Connected
        {
            get { return IsConnected; }
            set
            {
                if (value == IsConnected)
                    return;
                if (value)
                {
                    //Connect to the device, if connection fail - exception
                    if (CameraConnect() == false) throw new ASCOM.NotConnectedException("Cant connect to cam8");
                    //Show settings form
                    settings_form.Show();
                    //Read & set gain and offset parameters
                    m_Gain = short.Parse(settings_form.GainTextBox.Text);
                    m_Offset = short.Parse(settings_form.OffsetTextBox.Text);
                }
                else
                {
                    //Disconnect from the device
                    if (CameraDisconnect() == false) throw new ASCOM.NotConnectedException("Cant disconnect cam8");
                    //Hide settings form
                    settings_form.Hide();
                }
            }
        }

        public string Description
        {
            get { return driverDescription; }
        }

        public string DriverInfo
        {
            get { throw new ASCOM.PropertyNotImplementedException("DriverInfo", false); }
        }

        public string DriverVersion
        {
            get
            {
                Version version = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version;
                return String.Format(CultureInfo.InvariantCulture, "{0}.{55}", version.Major, version.Minor);
            }
        }

        public short InterfaceVersion
        {
            // set by the driver wizard
            get { return 2; }
        }

        public short BayerOffsetX
        {
            get { return 0; }
        }

        public short BayerOffsetY
        {
            get { return 0; }
        }

        public short BinX
        {
            get { return m_BinX; }
            set
            {
                if ((value <= this.MaxBinX) && (value >= 1)) m_BinX = value;
                else throw new ASCOM.InvalidValueException("Invalid BinX");
                m_StartX /= 2;
                m_NumX /= 2;
            }
        }

        public short BinY
        {
            get { return m_BinY; }
            set
            {
                if ((value <= this.MaxBinY) && (value >= 1)) m_BinY = value;
                else throw new ASCOM.InvalidValueException("Invalid BinY");
                m_StartY /= 2;
                m_NumY /= 2;
            }
        }

        public CameraStates CameraState
        {
            get
            {
                m_CameraState = (short)CameraGetCameraState();
                if (m_CameraState == 0) return CameraStates.cameraIdle;
                if (m_CameraState == 1) return CameraStates.cameraWaiting;
                if (m_CameraState == 2) return CameraStates.cameraExposing;
                if (m_CameraState == 3) return CameraStates.cameraReading;
                if (m_CameraState == 4) return CameraStates.cameraDownload;
                return CameraStates.cameraError;
            }
        }

        public bool CanAbortExposure
        {
            get { return true; }
        }

        public bool CanStopExposure
        {
            get { return true; }
        }

        public int CameraXSize
        {
            get { return CameraXSizeConst; }
        }

        public int CameraYSize
        {
            get { return CameraYSizeConst; }
        }

        public bool CanAsymmetricBin
        {
            get { return false; }
        }

        public bool CanGetCoolerPower
        {
            get { return false; }
        }

        public bool CanFastReadout
        {
            get { return false; }
        }

        public bool CanPulseGuide
        {
            get { return false; }
        }

        public bool CanSetCCDTemperature
        {
            get { return false; }
        }

        public double CCDTemperature
        {
            get { throw new ASCOM.InvalidValueException("get CCDTemperature: no CCDTemperature sensor"); }
        }

        public bool CoolerOn
        {
            get { throw new ASCOM.PropertyNotImplementedException("get CoolerOn: cant power on cooler"); }
            set { throw new ASCOM.PropertyNotImplementedException("set CoolerOnr: cant power on cooler"); }
        }

        public double CoolerPower
        {
            get { throw new ASCOM.PropertyNotImplementedException("get CoolerPower: cant get cooler power"); }
        }

        public double ElectronsPerADU
        {
            get { return 0.40; }
        }

        public double ExposureMax
        {
            get { return 36000.00; }
        }

        public double ExposureMin
        {
            get { return 0.00; }
        }

        public double ExposureResolution
        {
            get { return 0.01; }
        }

        public bool FastReadout
        {
            get { return false; }
            set { throw new ASCOM.PropertyNotImplementedException("set FastReadout: no fast readout mode now"); }
        }

        public double FullWellCapacity
        {
            get { return 25000.00; }
        }

        public short Gain
        {
            get { return m_Gain; }
            set
            {
                if ((value >= this.GainMin) && (value <= this.GainMax)) m_Gain = value;
                else throw new ASCOM.InvalidValueException("set Gain invalid gain value");
            }
        }

        public short GainMin
        {
            get { return 0; }
        }

        public short GainMax
        {
            get { return 63; }
        }

        public ArrayList Gains
        {
            get { return m_Gains; }
        }

        public bool HasShutter
        {
            get { return false; }
        }

        public double HeatSinkTemperature
        {
            get { throw new ASCOM.NotConnectedException("get HeatSinkTemperature: no HeatSink sensor"); }
        }

        public bool ImageReady
        {
            get
            {
                //get imare_ready flag from cam8ll055.dll
                m_ImageReady = CameraGetImageReady();
                //if image ready - mova image from memory to ASCOM container
                if (m_ImageReady)
                {
                    uint imagepoint;
                    //Get image pointer
                    imagepoint = CameraGetImage();
                    unsafe
                    {
                        int* zeropixelpoint, pixelpoint;
                        //Set pixelpointers
                        zeropixelpoint = pixelpoint = (int*)imagepoint;
                        //Create image array
                        m_Image = Array.CreateInstance(typeof(int), m_NumX * m_NumY);
                        int i, j, k = 0;
                        //If full CCD resolution - read CCD sequesly
                        if ((m_StartX == 0) && (m_StartY == 0) && (m_NumX == CameraXSizeConst) && (m_NumY == CameraYSizeConst))
                        {
                            for (j = m_StartY; j < (m_StartY + m_NumY); j++)
                                for (i = m_StartX; i < (m_StartX + m_NumX); i++)
                                {
                                    m_Image.SetValue(*pixelpoint, k);
                                    k++;
                                    pixelpoint++;
                                }
                        }
                        //If ROI is set - read ROI region of frame
                        else
                        {
                            for (j = m_StartY; j < (m_StartY + m_NumY); j++)
                                for (i = m_StartX; i < (m_StartX + m_NumX); i++)
                                {
                                    pixelpoint = (int*)(zeropixelpoint + (j * CameraXSizeConst + i));
                                    m_Image.SetValue(*pixelpoint, k);
                                    k++;
                                }
                        }
                    }
                }
                return m_ImageReady;
            }
        }

        public object ImageArray
        {
            get { return m_Image; }
        }

        public object ImageArrayVariant
        {
            get { return m_Image; }
        }

        public bool IsPulseGuiding
        {
            get { return false; }
        }

        public double LastExposureDuration
        {
            get { return m_LastExposureDuration; }
        }

        public string LastExposureStartTime
        {
            get { return m_LastExposureStartTime; }
        }

        public int MaxADU
        {
            get { return 65535; }
        }

        public short MaxBinX
        {
            get { return 2; }
        }

        public short MaxBinY
        {
            get { return 2; }
        }

        public string Name
        {
            get { return "cam8"; }
        }

        public int NumX
        {
            get { return m_NumX; }
            set
            {
                int temp;
                if (m_BinX == 1) temp = CameraXSizeConst - this.StartX;
                else temp = CameraXSizeConst / 2 - this.m_StartX;
                if ((value >= 0) && (value <= temp)) m_NumX = value;
                else throw new ASCOM.InvalidValueException("Invalid NumX");
            }
        }

        public int NumY
        {
            get { return m_NumY; }
            set
            {
                int temp;
                if (m_BinY == 1) temp = CameraYSizeConst - this.m_StartY;
                else temp = CameraYSizeConst / 2 - this.m_StartY;
                if ((value >= 0) && (value <= temp)) m_NumY = value;
                else throw new ASCOM.InvalidValueException("Invalid NumY");
            }
        }

        public short PercentCompleted
        {
            get { return 0; }
        }

        public double PixelSizeX
        {
            get { return 7.80 * m_BinX; }
        }

        public double PixelSizeY
        {
            get { return 7.80 * m_BinY; }
        }

        public short ReadoutMode
        {
            get { return m_ReadoutMode; }
            set { throw new ASCOM.MethodNotImplementedException("set ReadoutMode: Only 1 readout mode supported now"); }
        }

        public ArrayList ReadoutModes
        {
            get { return m_ReadoutModes; }
        }

        public string SensorName
        {
            get { return "ICX453AQ"; }
        }

        public SensorType SensorType
        {
            get { return SensorType.RGGB; }
        }

        public double SetCCDTemperature
        {
            get { throw new ASCOM.MethodNotImplementedException("get SetCCDTemperature: No CCD temperature control"); }
            set { throw new ASCOM.MethodNotImplementedException("set SetCCDTemperature: No CCD temperature control"); }
        }

        public int StartX
        {
            get { return m_StartX; }
            set
            {
                int temp;
                if (m_BinX == 1) temp = CameraXSizeConst;
                else temp = CameraXSizeConst / 2;
                if ((value >= 0) && (value < temp)) m_StartX = value;
                else throw new ASCOM.InvalidValueException("invalid StartX");
            }
        }

        public int StartY
        {
            get { return m_StartY; }
            set
            {
                int temp;
                if (m_BinY == 1) temp = CameraYSizeConst;
                else temp = CameraYSizeConst / 2;
                if ((value >= 0) && (value < temp)) m_StartY = value;
                else throw new ASCOM.InvalidValueException("invalid StartY");
            }
        }

        #endregion

        #region private properties and methods
        // here are some useful properties and methods that can be used as required
        // to help with

        /// <summary>
        /// Returns true if there is a valid connection to the driver hardware
        /// </summary>
        private bool IsConnected
        {
            get
            {
                //check that the driver hardware connection exists and is connected to the hardware     
                return (CameraIsConnected());
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
                throw new ASCOM.NotConnectedException(message);
            }
        }
        #endregion
    }
}
