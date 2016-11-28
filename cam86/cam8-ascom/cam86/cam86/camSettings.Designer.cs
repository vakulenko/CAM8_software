namespace ASCOM.cam86
{
    partial class camSettings
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.gainTrackBar = new System.Windows.Forms.TrackBar();
            this.offsetTrackBar = new System.Windows.Forms.TrackBar();
            this.minMaxOffsetLabel = new System.Windows.Forms.Label();
            this.minMaxGainLabel = new System.Windows.Forms.Label();
            this.offsetLabel = new System.Windows.Forms.Label();
            this.gainLabel = new System.Windows.Forms.Label();
            this.onTopCheckBox = new System.Windows.Forms.CheckBox();
            this.gainNumUpDown = new System.Windows.Forms.NumericUpDown();
            this.offsetNumUpDown = new System.Windows.Forms.NumericUpDown();
            this.cameraStatusLabel = new System.Windows.Forms.Label();
            this.slowCoolingCheckBox = new System.Windows.Forms.CheckBox();
            this.slowCoolingLabel = new System.Windows.Forms.Label();
            this.slowCoolingNumUpDown = new System.Windows.Forms.NumericUpDown();
            ((System.ComponentModel.ISupportInitialize)(this.gainTrackBar)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.offsetTrackBar)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.gainNumUpDown)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.offsetNumUpDown)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.slowCoolingNumUpDown)).BeginInit();
            this.SuspendLayout();
            // 
            // gainTrackBar
            // 
            this.gainTrackBar.Location = new System.Drawing.Point(6, 25);
            this.gainTrackBar.Maximum = 63;
            this.gainTrackBar.Name = "gainTrackBar";
            this.gainTrackBar.Size = new System.Drawing.Size(219, 42);
            this.gainTrackBar.TabIndex = 36;
            this.gainTrackBar.Scroll += new System.EventHandler(this.GainTrackBar_Scroll);
            // 
            // offsetTrackBar
            // 
            this.offsetTrackBar.Location = new System.Drawing.Point(6, 91);
            this.offsetTrackBar.Maximum = 127;
            this.offsetTrackBar.Minimum = -127;
            this.offsetTrackBar.Name = "offsetTrackBar";
            this.offsetTrackBar.Size = new System.Drawing.Size(219, 42);
            this.offsetTrackBar.TabIndex = 35;
            this.offsetTrackBar.Scroll += new System.EventHandler(this.OffsetTrackBar_Scroll);
            // 
            // minMaxOffsetLabel
            // 
            this.minMaxOffsetLabel.AutoSize = true;
            this.minMaxOffsetLabel.Location = new System.Drawing.Point(47, 73);
            this.minMaxOffsetLabel.Name = "minMaxOffsetLabel";
            this.minMaxOffsetLabel.Size = new System.Drawing.Size(52, 13);
            this.minMaxOffsetLabel.TabIndex = 34;
            this.minMaxOffsetLabel.Text = "-127..127";
            // 
            // minMaxGainLabel
            // 
            this.minMaxGainLabel.AutoSize = true;
            this.minMaxGainLabel.Location = new System.Drawing.Point(47, 7);
            this.minMaxGainLabel.Name = "minMaxGainLabel";
            this.minMaxGainLabel.Size = new System.Drawing.Size(31, 13);
            this.minMaxGainLabel.TabIndex = 33;
            this.minMaxGainLabel.Text = "0..63";
            // 
            // offsetLabel
            // 
            this.offsetLabel.AutoSize = true;
            this.offsetLabel.Location = new System.Drawing.Point(6, 73);
            this.offsetLabel.Name = "offsetLabel";
            this.offsetLabel.Size = new System.Drawing.Size(35, 13);
            this.offsetLabel.TabIndex = 32;
            this.offsetLabel.Text = "Offset";
            // 
            // gainLabel
            // 
            this.gainLabel.AutoSize = true;
            this.gainLabel.Location = new System.Drawing.Point(6, 7);
            this.gainLabel.Name = "gainLabel";
            this.gainLabel.Size = new System.Drawing.Size(29, 13);
            this.gainLabel.TabIndex = 29;
            this.gainLabel.Text = "Gain";
            // 
            // onTopCheckBox
            // 
            this.onTopCheckBox.AutoSize = true;
            this.onTopCheckBox.Location = new System.Drawing.Point(163, 7);
            this.onTopCheckBox.Name = "onTopCheckBox";
            this.onTopCheckBox.Size = new System.Drawing.Size(62, 17);
            this.onTopCheckBox.TabIndex = 37;
            this.onTopCheckBox.Text = "On Top";
            this.onTopCheckBox.UseVisualStyleBackColor = true;
            this.onTopCheckBox.CheckedChanged += new System.EventHandler(this.OnTopCheckBox_CheckedChanged);
            // 
            // gainNumUpDown
            // 
            this.gainNumUpDown.Location = new System.Drawing.Point(103, 4);
            this.gainNumUpDown.Maximum = new decimal(new int[] {
            63,
            0,
            0,
            0});
            this.gainNumUpDown.Name = "gainNumUpDown";
            this.gainNumUpDown.Size = new System.Drawing.Size(51, 20);
            this.gainNumUpDown.TabIndex = 42;
            this.gainNumUpDown.ValueChanged += new System.EventHandler(this.gainNumUpDown_ValueChanged);
            // 
            // offsetNumUpDown
            // 
            this.offsetNumUpDown.Location = new System.Drawing.Point(103, 70);
            this.offsetNumUpDown.Maximum = new decimal(new int[] {
            127,
            0,
            0,
            0});
            this.offsetNumUpDown.Minimum = new decimal(new int[] {
            127,
            0,
            0,
            -2147483648});
            this.offsetNumUpDown.Name = "offsetNumUpDown";
            this.offsetNumUpDown.Size = new System.Drawing.Size(51, 20);
            this.offsetNumUpDown.TabIndex = 43;
            this.offsetNumUpDown.ValueChanged += new System.EventHandler(this.offsetNumUpDown_ValueChanged);
            // 
            // cameraStatusLabel
            // 
            this.cameraStatusLabel.AutoSize = true;
            this.cameraStatusLabel.Location = new System.Drawing.Point(6, 155);
            this.cameraStatusLabel.Name = "cameraStatusLabel";
            this.cameraStatusLabel.Size = new System.Drawing.Size(132, 13);
            this.cameraStatusLabel.TabIndex = 45;
            this.cameraStatusLabel.Text = "Camera status: operational";
            // 
            // slowCoolingCheckBox
            // 
            this.slowCoolingCheckBox.AutoSize = true;
            this.slowCoolingCheckBox.Location = new System.Drawing.Point(9, 131);
            this.slowCoolingCheckBox.Name = "slowCoolingCheckBox";
            this.slowCoolingCheckBox.Size = new System.Drawing.Size(87, 17);
            this.slowCoolingCheckBox.TabIndex = 50;
            this.slowCoolingCheckBox.Text = "Slow Cooling";
            this.slowCoolingCheckBox.UseVisualStyleBackColor = true;
            // 
            // slowCoolingLabel
            // 
            this.slowCoolingLabel.AutoSize = true;
            this.slowCoolingLabel.Location = new System.Drawing.Point(159, 133);
            this.slowCoolingLabel.Name = "slowCoolingLabel";
            this.slowCoolingLabel.Size = new System.Drawing.Size(35, 13);
            this.slowCoolingLabel.TabIndex = 51;
            this.slowCoolingLabel.Text = "C/min";
            // 
            // slowCoolingNumUpDown
            // 
            this.slowCoolingNumUpDown.DecimalPlaces = 1;
            this.slowCoolingNumUpDown.Increment = new decimal(new int[] {
            1,
            0,
            0,
            65536});
            this.slowCoolingNumUpDown.Location = new System.Drawing.Point(102, 130);
            this.slowCoolingNumUpDown.Maximum = new decimal(new int[] {
            2,
            0,
            0,
            0});
            this.slowCoolingNumUpDown.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            65536});
            this.slowCoolingNumUpDown.Name = "slowCoolingNumUpDown";
            this.slowCoolingNumUpDown.Size = new System.Drawing.Size(51, 20);
            this.slowCoolingNumUpDown.TabIndex = 52;
            this.slowCoolingNumUpDown.Value = new decimal(new int[] {
            5,
            0,
            0,
            65536});
            // 
            // camSettings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(230, 183);
            this.Controls.Add(this.slowCoolingNumUpDown);
            this.Controls.Add(this.slowCoolingLabel);
            this.Controls.Add(this.slowCoolingCheckBox);
            this.Controls.Add(this.cameraStatusLabel);
            this.Controls.Add(this.offsetNumUpDown);
            this.Controls.Add(this.gainNumUpDown);
            this.Controls.Add(this.onTopCheckBox);
            this.Controls.Add(this.gainTrackBar);
            this.Controls.Add(this.offsetTrackBar);
            this.Controls.Add(this.minMaxOffsetLabel);
            this.Controls.Add(this.minMaxGainLabel);
            this.Controls.Add(this.offsetLabel);
            this.Controls.Add(this.gainLabel);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.Fixed3D;
            this.MaximizeBox = false;
            this.Name = "camSettings";
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.Text = "CAM86 settings";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.camSettings_FormClosing);
            ((System.ComponentModel.ISupportInitialize)(this.gainTrackBar)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.offsetTrackBar)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.gainNumUpDown)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.offsetNumUpDown)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.slowCoolingNumUpDown)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TrackBar gainTrackBar;
        private System.Windows.Forms.TrackBar offsetTrackBar;
        private System.Windows.Forms.Label minMaxOffsetLabel;
        private System.Windows.Forms.Label minMaxGainLabel;
        private System.Windows.Forms.Label offsetLabel;
        private System.Windows.Forms.Label gainLabel;
        private System.Windows.Forms.CheckBox onTopCheckBox;
        private System.Windows.Forms.NumericUpDown gainNumUpDown;
        private System.Windows.Forms.NumericUpDown offsetNumUpDown;
        private System.Windows.Forms.Label cameraStatusLabel;
        private System.Windows.Forms.CheckBox slowCoolingCheckBox;
        private System.Windows.Forms.Label slowCoolingLabel;
        private System.Windows.Forms.NumericUpDown slowCoolingNumUpDown;
    }
}