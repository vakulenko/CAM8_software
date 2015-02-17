using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO; // work with files
using System.Xml.Serialization; // class savings

namespace ASCOM.cam8_v055
{
    public partial class cam_settings : Form
    {
        const short MinGain = 0;
        const short MaxGain = 63;
        const short MinOffset = -127;
        const short MaxOffset = 127;
        string SettingFilePath = "";

        public cam_settings()
        {
            InitializeComponent();

            string arch = System.Environment.GetEnvironmentVariable("PROCESSOR_ARCHITECTURE").ToString();
            if (arch.IndexOf("86") != -1) SettingFilePath = Environment.ExpandEnvironmentVariables("%CommonProgramFiles%\\ASCOM\\Camera\\cam8\\cam8_v05.xml");
            else SettingFilePath = Environment.ExpandEnvironmentVariables("%CommonProgramFiles(x86)%\\ASCOM\\Camera\\cam8\\cam8_v05.xml");

            //extract gain, offset settings
            if (File.Exists(SettingFilePath))
            {
                try
                {

                    using (Stream stream = new FileStream(SettingFilePath, FileMode.Open))
                    {
                        XmlSerializer serializer = new XmlSerializer(typeof(iniSettings));

                        iniSettings iniSet = (iniSettings)serializer.Deserialize(stream);
                        //check gain/offset validity
                        if ((iniSet.gain < 0) || (iniSet.gain > 63)) iniSet.gain = 0;
                        if ((iniSet.offset < -127) || (iniSet.offset > 127)) iniSet.offset = 0;
                        GainTrackBar.Value = iniSet.gain;
                        OffsetTrackBar.Value = iniSet.offset;
                        GainTextBox.Text = iniSet.gain.ToString();
                        OffsetTextBox.Text = iniSet.offset.ToString();
                    }
                }
                catch
                {
                    System.Windows.Forms.MessageBox.Show(SettingFilePath+ " damaged, use settings by default.");
                    GainTrackBar.Value = 34;
                    OffsetTrackBar.Value = -7;
                    GainTextBox.Text = "34";
                    OffsetTextBox.Text = "-7";                    
                }
            }
        }

        private void GainTrackBar_Scroll(object sender, EventArgs e)
        {
            GainTextBox.Text = GainTrackBar.Value.ToString();
        }

        private void OffsetTrackBar_Scroll(object sender, EventArgs e)
        {
            OffsetTextBox.Text = OffsetTrackBar.Value.ToString();
        }

        private void GainTextBox_TextChanged(object sender, EventArgs e)
        {
            bool ConvRes;
            short ValNum;
            //Settings are correct?
            ConvRes = short.TryParse(GainTextBox.Text, out ValNum);
            if ((ConvRes == false) || (ValNum < 0) || (ValNum > 63))
            {
                GainTrackBar.Value = MinGain;
                GainTextBox.Text = MinGain.ToString();
                return;
            }
            GainTrackBar.Value = short.Parse(GainTextBox.Text);
        }

        private void OffsetTextBox_TextChanged(object sender, EventArgs e)
        {
            bool ConvRes;
            short ValNum;
            //Settings are correct?
            ConvRes = short.TryParse(OffsetTextBox.Text, out ValNum);
            if ((ConvRes == false) || (ValNum < -127) || (ValNum > 127))
            {
                OffsetTrackBar.Value = MinOffset;
                OffsetTextBox.Text = MinOffset.ToString();
                return;
            }
            OffsetTrackBar.Value = short.Parse(OffsetTextBox.Text);
        }

        private void OnTopCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (OnTopCheckBox.Checked) this.TopMost = true;
            else this.TopMost = false;
        }
    }
}
