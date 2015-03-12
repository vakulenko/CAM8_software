using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO.Ports;
using System.IO;
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
                
                deviceComPort = new SerialPort(comPortComboBox.SelectedItem.ToString(), 38400, System.IO.Ports.Parity.None, 8, System.IO.Ports.StopBits.One);
                                
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
                    ftdiDevice.SetTimeouts(10000, 10000);
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
            byte[] cmd = { 0x33};         
            string readData;
            UInt32 numBytesRead = 0, x, summ=0;

            using (StreamWriter writer = new StreamWriter("debug.txt"))
            {
                deviceComPort.Write(cmd, 0, cmd.Length);
                for (x = 0; x < 200; x++)
                {
                    ftdiDevice.Read(out readData, 60000, ref numBytesRead);
                    summ = summ + numBytesRead;
                    writer.WriteLine(x.ToString()+" "+numBytesRead.ToString());
                }
                writer.WriteLine("summary: "+summ.ToString()+"bytes");
                listBox.Items.Add("Received " + summ.ToString() + " bytes from FT2232H");                
            }            
        }
    }
}
