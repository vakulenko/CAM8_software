namespace ASCOM.cam8_v05
{
    partial class cam_settings
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
            this.GainTrackBar = new System.Windows.Forms.TrackBar();
            this.OffsetTrackBar = new System.Windows.Forms.TrackBar();
            this.MinMaxOffsetLabel = new System.Windows.Forms.Label();
            this.MinMaxGainLabel = new System.Windows.Forms.Label();
            this.OffsetLabel = new System.Windows.Forms.Label();
            this.OffsetTextBox = new System.Windows.Forms.TextBox();
            this.GainTextBox = new System.Windows.Forms.TextBox();
            this.GainLabel = new System.Windows.Forms.Label();
            this.OnTopCheckBox = new System.Windows.Forms.CheckBox();
            ((System.ComponentModel.ISupportInitialize)(this.GainTrackBar)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.OffsetTrackBar)).BeginInit();
            this.SuspendLayout();
            // 
            // GainTrackBar
            // 
            this.GainTrackBar.Location = new System.Drawing.Point(6, 25);
            this.GainTrackBar.Maximum = 63;
            this.GainTrackBar.Name = "GainTrackBar";
            this.GainTrackBar.Size = new System.Drawing.Size(219, 45);
            this.GainTrackBar.TabIndex = 36;
            this.GainTrackBar.Value = 34;
            this.GainTrackBar.Scroll += new System.EventHandler(this.GainTrackBar_Scroll);
            // 
            // OffsetTrackBar
            // 
            this.OffsetTrackBar.Location = new System.Drawing.Point(6, 92);
            this.OffsetTrackBar.Maximum = 127;
            this.OffsetTrackBar.Minimum = -127;
            this.OffsetTrackBar.Name = "OffsetTrackBar";
            this.OffsetTrackBar.Size = new System.Drawing.Size(219, 45);
            this.OffsetTrackBar.TabIndex = 35;
            this.OffsetTrackBar.Value = -7;
            this.OffsetTrackBar.Scroll += new System.EventHandler(this.OffsetTrackBar_Scroll);
            // 
            // MinMaxOffsetLabel
            // 
            this.MinMaxOffsetLabel.AutoSize = true;
            this.MinMaxOffsetLabel.Location = new System.Drawing.Point(47, 74);
            this.MinMaxOffsetLabel.Name = "MinMaxOffsetLabel";
            this.MinMaxOffsetLabel.Size = new System.Drawing.Size(52, 13);
            this.MinMaxOffsetLabel.TabIndex = 34;
            this.MinMaxOffsetLabel.Text = "-127..127";
            // 
            // MinMaxGainLabel
            // 
            this.MinMaxGainLabel.AutoSize = true;
            this.MinMaxGainLabel.Location = new System.Drawing.Point(47, 7);
            this.MinMaxGainLabel.Name = "MinMaxGainLabel";
            this.MinMaxGainLabel.Size = new System.Drawing.Size(31, 13);
            this.MinMaxGainLabel.TabIndex = 33;
            this.MinMaxGainLabel.Text = "0..63";
            // 
            // OffsetLabel
            // 
            this.OffsetLabel.AutoSize = true;
            this.OffsetLabel.Location = new System.Drawing.Point(6, 74);
            this.OffsetLabel.Name = "OffsetLabel";
            this.OffsetLabel.Size = new System.Drawing.Size(35, 13);
            this.OffsetLabel.TabIndex = 32;
            this.OffsetLabel.Text = "Offset";
            // 
            // OffsetTextBox
            // 
            this.OffsetTextBox.Location = new System.Drawing.Point(106, 70);
            this.OffsetTextBox.Name = "OffsetTextBox";
            this.OffsetTextBox.Size = new System.Drawing.Size(51, 20);
            this.OffsetTextBox.TabIndex = 31;
            this.OffsetTextBox.Text = "-7";
            this.OffsetTextBox.TextChanged += new System.EventHandler(this.OffsetTextBox_TextChanged);
            // 
            // GainTextBox
            // 
            this.GainTextBox.Location = new System.Drawing.Point(106, 4);
            this.GainTextBox.Name = "GainTextBox";
            this.GainTextBox.Size = new System.Drawing.Size(51, 20);
            this.GainTextBox.TabIndex = 30;
            this.GainTextBox.Text = "34";
            this.GainTextBox.TextChanged += new System.EventHandler(this.GainTextBox_TextChanged);
            // 
            // GainLabel
            // 
            this.GainLabel.AutoSize = true;
            this.GainLabel.Location = new System.Drawing.Point(6, 7);
            this.GainLabel.Name = "GainLabel";
            this.GainLabel.Size = new System.Drawing.Size(29, 13);
            this.GainLabel.TabIndex = 29;
            this.GainLabel.Text = "Gain";
            // 
            // OnTopCheckBox
            // 
            this.OnTopCheckBox.AutoSize = true;
            this.OnTopCheckBox.Location = new System.Drawing.Point(163, 7);
            this.OnTopCheckBox.Name = "OnTopCheckBox";
            this.OnTopCheckBox.Size = new System.Drawing.Size(62, 17);
            this.OnTopCheckBox.TabIndex = 37;
            this.OnTopCheckBox.Text = "On Top";
            this.OnTopCheckBox.UseVisualStyleBackColor = true;
            this.OnTopCheckBox.CheckedChanged += new System.EventHandler(this.OnTopCheckBox_CheckedChanged);
            // 
            // cam_settings
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(230, 133);
            this.ControlBox = false;
            this.Controls.Add(this.OnTopCheckBox);
            this.Controls.Add(this.GainTrackBar);
            this.Controls.Add(this.OffsetTrackBar);
            this.Controls.Add(this.MinMaxOffsetLabel);
            this.Controls.Add(this.MinMaxGainLabel);
            this.Controls.Add(this.OffsetLabel);
            this.Controls.Add(this.OffsetTextBox);
            this.Controls.Add(this.GainTextBox);
            this.Controls.Add(this.GainLabel);
            this.MaximumSize = new System.Drawing.Size(238, 167);
            this.MinimumSize = new System.Drawing.Size(238, 0);
            this.Name = "cam_settings";
            this.Text = "CAM8_settings";
            ((System.ComponentModel.ISupportInitialize)(this.GainTrackBar)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.OffsetTrackBar)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TrackBar GainTrackBar;
        private System.Windows.Forms.TrackBar OffsetTrackBar;
        private System.Windows.Forms.Label MinMaxOffsetLabel;
        private System.Windows.Forms.Label MinMaxGainLabel;
        private System.Windows.Forms.Label OffsetLabel;
        public System.Windows.Forms.TextBox OffsetTextBox;
        public System.Windows.Forms.TextBox GainTextBox;
        private System.Windows.Forms.Label GainLabel;
        private System.Windows.Forms.CheckBox OnTopCheckBox;
    }
}