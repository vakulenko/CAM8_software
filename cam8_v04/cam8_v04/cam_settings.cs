using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ASCOM.cam8_v04
{
    public partial class cam_settings : Form
    {
        const short MinGain = 0;
        const short MaxGain = 63;
        const short MinOffset = -127;
        const short MaxOffset = 127;

        public cam_settings()
        {
            InitializeComponent();
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
            if (OnTopCheckBox.Checked)    this.TopMost = true;
                                     else this.TopMost = false;    
        }
    }
}
