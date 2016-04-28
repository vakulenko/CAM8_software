using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace ASCOM.cam85_v03
{
    public partial class camSettings : Form
    {
        const short minGain = 0;
        const short maxGain = 63;
        const short minOffset = -127;
        const short maxOffset = 127;
        const int minBaudrateAdjust = 120;
        const int maxBaudrateAdjust = 200;

        const short CameraStatusOperational = 0;
        const short CameraStatusWarning = 1;
        const short CameraStatusFailed = 2;

        public camSettings()
        {
            InitializeComponent();
        }

        private short pGain = 0;
        private short pOffset = 0;
        private int pBaudrateAdjust = 200;

        public short gain
        {
            get
            {
                return pGain;
            }
            set
            {
                if ((value >= minGain) && (value <= maxGain))
                {
                    pGain = value;
                    this.gainNumUpDown.Value = pGain;
                    this.gainTrackBar.Value = pGain;
                }
                else throw new ASCOM.InvalidValueException("cam_settings, gain");
            }
        }
        public short offset
        {
            get
            {
                return pOffset;
            }
            set
            {
                if ((value >= minOffset) && (value <= maxOffset))
                {
                    pOffset = value;
                    this.offsetNumUpDown.Value = pOffset;
                    this.offsetTrackBar.Value = pOffset;
                }
                else throw new ASCOM.InvalidValueException("cam_settings, offset");
            }
        }

        public string tecStatus
        {
            set
            {
                tecStatusLabel.Text = "TEC status: " + value;
            }
        }

        public bool onTop
        {
            get
            {
                return onTopCheckBox.Checked;
            }
            set
            {
                onTopCheckBox.Checked = value;
            }
        }

        public int cameraError
        {
            set
            {
                switch (value)
                {
                    case CameraStatusOperational:
                        {
                            this.BackColor = SystemColors.Control;
                            this.cameraStatusLabel.Text = "Camera status: operational";
                            break;
                        };
                    case CameraStatusWarning:
                        {
                            this.BackColor = System.Drawing.Color.Yellow;
                            this.cameraStatusLabel.Text = "Camera status: warning";
                            break;
                        };
                    default:
                        {
                            this.BackColor = System.Drawing.Color.Yellow;
                            this.cameraStatusLabel.Text = "Camera status: failed";
                            break;
                        };
                }
            }
        }

        public int baudrateAdjust
        {
            get
            {
                return pBaudrateAdjust;
            }
            set
            {
                if ((value >= minBaudrateAdjust) && (value <= maxBaudrateAdjust))
                {
                    pBaudrateAdjust = value;
                    this.baudrateAdjustNumUpDown.Value = pBaudrateAdjust;
                }
                else throw new ASCOM.InvalidValueException("cam_settings, baudrateAdjust");
            }
        }

        private void GainTrackBar_Scroll(object sender, EventArgs e)
        {
            gainNumUpDown.Value = gainTrackBar.Value;
        }

        private void OffsetTrackBar_Scroll(object sender, EventArgs e)
        {
            offsetNumUpDown.Value = offsetTrackBar.Value;
        }

        private void OnTopCheckBox_CheckedChanged(object sender, EventArgs e)
        {
            if (onTopCheckBox.Checked) this.TopMost = true;
            else this.TopMost = false;
        }

        private void gainNumUpDown_ValueChanged(object sender, EventArgs e)
        {
            gainTrackBar.Value = gain = (short)gainNumUpDown.Value;
        }

        private void offsetNumUpDown_ValueChanged(object sender, EventArgs e)
        {
            offsetTrackBar.Value = offset = (short)offsetNumUpDown.Value;
        }

        private void baudrateAdjustNumUpDown_ValueChanged(object sender, EventArgs e)
        {
            int diff = 40;
            for (int i = minBaudrateAdjust; i <= maxBaudrateAdjust; i += 40)
            {
                if (Math.Abs(i - baudrateAdjustNumUpDown.Value) <= Math.Abs(diff))
                {
                    diff = (int)(i - baudrateAdjustNumUpDown.Value);
                }
            }
            baudrateAdjustNumUpDown.Value = baudrateAdjustNumUpDown.Value + diff;
            baudrateAdjust = (int)baudrateAdjustNumUpDown.Value;
        }

        private void camSettings_FormClosing(object sender, FormClosingEventArgs e)
        {
            e.Cancel = true;
        }
    }
}
