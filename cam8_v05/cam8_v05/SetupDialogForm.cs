using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Drawing;
using System.Runtime.InteropServices;
using System.Text;
using System.Windows.Forms;

using FTD2XX_NET;

namespace ASCOM.cam8_v05
{
    [ComVisible(false)]					// Form not registered for COM!
    public partial class SetupDialogForm : Form
    {
        const short MinGain = 0;
        const short MaxGain = 63;
        const short MinOffset = -127;
        const short MaxOffset = 127;

        public SetupDialogForm()
        {
            InitializeComponent();
            //Initialize components;

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
                else throw new ASCOM.InvalidValueException("Error getting parameters from FTDI devices");
            }
            //Close device
            ftStatus = tempFtdiDevice.Close();
        }

        private void cmdOK_Click(object sender, EventArgs e)
        {
            Dispose();
            Close();
        }

        private void cmdCancel_Click(object sender, EventArgs e)
        {
            Close();
        }

        private void BrowseToAscom(object sender, EventArgs e)
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
    }
}