using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;
using ASCOM.Utilities;
using System.IO.Ports;
using ASCOM.cam8s_v06;

using FTD2XX_NET;

namespace ASCOM.cam8s_v06
{
    [ComVisible(false)]					// Form not registered for COM!
    public partial class SetupDialogForm : Form
    {
        public SetupDialogForm()
        {
            InitializeComponent();

            UInt32 ftdiDeviceCount = 0;
            FTDI.FT_STATUS ftStatus = FTDI.FT_STATUS.FT_OK;
            // Create new instance of the FTDI device class
            FTDI tempFtdiDevice = new FTDI();
            // Determine the number of FTDI devices connected to the machine
            ftStatus = tempFtdiDevice.GetNumberOfDevices(ref ftdiDeviceCount);
            // Check status
            if (ftStatus == FTDI.FT_STATUS.FT_OK)
                AvailableDevicesListBox.Items.Add("# of FTDI devices = " + ftdiDeviceCount.ToString());
            else
                throw new ASCOM.InvalidValueException("Error getting count FTDI devices");
            if (ftdiDeviceCount > 0)
            {
                // Allocate storage for device info list
                FTDI.FT_DEVICE_INFO_NODE[] ftdiDeviceList = new FTDI.FT_DEVICE_INFO_NODE[ftdiDeviceCount];
                // Populate our device list
                ftStatus = tempFtdiDevice.GetDeviceList(ftdiDeviceList);
                //Show device properties
                if (ftStatus == FTDI.FT_STATUS.FT_OK)
                {
                    for (UInt32 i = 0; i < ftdiDeviceCount; i++)
                    {
                        if (ftdiDeviceList[i].SerialNumber.Contains("CAM8"))
                        {
                            AvailableDevicesListBox.Items.Add("Device Index: " + i.ToString());
                            AvailableDevicesListBox.Items.Add("Flags: " + String.Format("{0:x}", ftdiDeviceList[i].Flags));
                            AvailableDevicesListBox.Items.Add("Type: " + ftdiDeviceList[i].Type.ToString());
                            AvailableDevicesListBox.Items.Add("ID: " + String.Format("{0:x}", ftdiDeviceList[i].ID));
                            AvailableDevicesListBox.Items.Add("Location ID: " + String.Format("{0:x}", ftdiDeviceList[i].LocId));
                            AvailableDevicesListBox.Items.Add("Serial Number: " + ftdiDeviceList[i].SerialNumber.ToString());
                            AvailableDevicesListBox.Items.Add("Description: " + ftdiDeviceList[i].Description.ToString());
                            AvailableDevicesListBox.Items.Add("");
                        }
                    }
                }
                else throw new ASCOM.InvalidValueException("Error getting parameters from FTDI devices");
            }
            //Close device
            ftStatus = tempFtdiDevice.Close();

            // Initialise current values of user settings from the ASCOM Profile 
            chkTrace.Checked = Camera.traceState;
            coolerCheckBox.Checked = Camera.coolerEnabledState;
            //find available com ports in system
            string[] comPorts;
            comPorts = SerialPort.GetPortNames();
            int j;
            for (j = 0; j < comPorts.Length; j++)
            {
                coolerComPortComboBox.Items.Add(comPorts[j]);
                if (comPorts[j] == Camera.coolerComPortState) coolerComPortComboBox.SelectedIndex = j;
            }

        }

        private void cmdOK_Click(object sender, EventArgs e) // OK button event handler
        {
            Camera.traceState = chkTrace.Checked;
            Camera.coolerEnabledState = coolerCheckBox.Checked;
            if (coolerComPortComboBox.SelectedItem != null) Camera.coolerComPortState = coolerComPortComboBox.SelectedItem.ToString();
            else Camera.coolerEnabledState = false;
        }

        private void cmdCancel_Click(object sender, EventArgs e) // Cancel button event handler
        {
            Close();
        }

        private void BrowseToAscom(object sender, EventArgs e) // Click on ASCOM logo event handler
        {
            try
            {
                System.Diagnostics.Process.Start("http://ascom-standards.org/");
            }
            catch (System.ComponentModel.Win32Exception noBrowser)
            {
                if (noBrowser.ErrorCode == -2147467259)
                    MessageBox.Show(noBrowser.Message);
            }
            catch (System.Exception other)
            {
                MessageBox.Show(other.Message);
            }
        }

        private void coolerCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            coolerComPortComboBox.Enabled = coolerCheckBox.Checked;
        }
    }
}