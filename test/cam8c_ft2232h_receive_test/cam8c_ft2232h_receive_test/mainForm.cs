using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO.Ports;
using System.Threading;

using FTD2XX_NET;

namespace cam8c_ft2232h_receive_test
{
    public partial class mainForm : Form
    {
        private bool isConnected = false;
        SerialPort deviceComPort;
        FTDI ftdiDevice;

        public mainForm()
        {
            InitializeComponent();

            string[] comPorts;
            comPorts = SerialPort.GetPortNames();
            int j;
            for (j = 0; j < comPorts.Length; j++)            
                comPortComboBox.Items.Add(comPorts[j]);

            ftdiDevice = new FTDI();
        }

        private void connectBtn_Click(object sender, EventArgs e)
        {
            if (isConnected)
            {
                deviceComPort.Close();
                ftdiDevice.Close();
                isConnected = false;
                connectBtn.Text = "Connect";
                readframeBtn.Enabled = false;
            }
            else
            {                
                deviceComPort = new SerialPort(comPortComboBox.SelectedItem.ToString(), 9600, System.IO.Ports.Parity.None, 8, System.IO.Ports.StopBits.One);
                                
                try
                {
                    deviceComPort.Open();
                    deviceComPort.DiscardInBuffer();
                    deviceComPort.DiscardOutBuffer();
                    deviceComPort.ReadTimeout = 500;
                    deviceComPort.WriteTimeout = 500;
                }
                catch
                {
                    isConnected = false;
                    System.Windows.Forms.MessageBox.Show("COM PORT. Connection failed");
                    return;
                }
                
                try
                {
                    ftdiDevice.OpenBySerialNumber("CAM8CA");
                    ftdiDevice.SetLatency(2);
                    ftdiDevice.SetTimeouts(4000, 4000);
                    ftdiDevice.Purge(FTDI.FT_PURGE.FT_PURGE_RX | FTDI.FT_PURGE.FT_PURGE_TX);
                }
                catch
                {
                    deviceComPort.Close();
                    isConnected = false;
                    System.Windows.Forms.MessageBox.Show("FTDI. Connection failed");
                    return;
                } 
                connectBtn.Text = "Disconnect";
                readframeBtn.Enabled = true;
                isConnected = true;
            }
        }

        private void readframeBtn_Click(object sender, EventArgs e)
        {
            uint numBytesAvailable=0;

            byte[] cmd = { 0x33 };
            deviceComPort.Write(cmd, 0, cmd.Length);
            //read ftdi status
            ftdiDevice.GetRxBytesAvailable(ref numBytesAvailable);
            outLabel.Text = "IN Buffer contains "+numBytesAvailable.ToString()+ " bytes";
        }
    }
}
