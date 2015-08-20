namespace ASCOM.cam8s_v08
{
    partial class SetupDialogForm
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
            this.cmdOK = new System.Windows.Forms.Button();
            this.cmdCancel = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.picASCOM = new System.Windows.Forms.PictureBox();
            this.chkTrace = new System.Windows.Forms.CheckBox();
            this.AvailableDevicesListBox = new System.Windows.Forms.ListBox();
            this.coolerGroupBox = new System.Windows.Forms.GroupBox();
            this.coolerComPortComboBox = new System.Windows.Forms.ComboBox();
            this.coolerCheckBox = new System.Windows.Forms.CheckBox();
            ((System.ComponentModel.ISupportInitialize)(this.picASCOM)).BeginInit();
            this.coolerGroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // cmdOK
            // 
            this.cmdOK.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.cmdOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.cmdOK.Location = new System.Drawing.Point(210, 262);
            this.cmdOK.Name = "cmdOK";
            this.cmdOK.Size = new System.Drawing.Size(119, 24);
            this.cmdOK.TabIndex = 0;
            this.cmdOK.Text = "OK";
            this.cmdOK.UseVisualStyleBackColor = true;
            this.cmdOK.Click += new System.EventHandler(this.cmdOK_Click);
            // 
            // cmdCancel
            // 
            this.cmdCancel.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.cmdCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.cmdCancel.Location = new System.Drawing.Point(210, 292);
            this.cmdCancel.Name = "cmdCancel";
            this.cmdCancel.Size = new System.Drawing.Size(119, 25);
            this.cmdCancel.TabIndex = 1;
            this.cmdCancel.Text = "Cancel";
            this.cmdCancel.UseVisualStyleBackColor = true;
            this.cmdCancel.Click += new System.EventHandler(this.cmdCancel_Click);
            // 
            // label1
            // 
            this.label1.Location = new System.Drawing.Point(12, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(123, 20);
            this.label1.TabIndex = 2;
            this.label1.Text = "Available devices";
            // 
            // picASCOM
            // 
            this.picASCOM.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.picASCOM.Cursor = System.Windows.Forms.Cursors.Hand;
            this.picASCOM.Image = global::ASCOM.cam8s_v08.Properties.Resources.ASCOM;
            this.picASCOM.Location = new System.Drawing.Point(242, 59);
            this.picASCOM.Name = "picASCOM";
            this.picASCOM.Size = new System.Drawing.Size(48, 56);
            this.picASCOM.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize;
            this.picASCOM.TabIndex = 3;
            this.picASCOM.TabStop = false;
            this.picASCOM.Click += new System.EventHandler(this.BrowseToAscom);
            this.picASCOM.DoubleClick += new System.EventHandler(this.BrowseToAscom);
            // 
            // chkTrace
            // 
            this.chkTrace.AutoSize = true;
            this.chkTrace.Location = new System.Drawing.Point(216, 239);
            this.chkTrace.Name = "chkTrace";
            this.chkTrace.Size = new System.Drawing.Size(69, 17);
            this.chkTrace.TabIndex = 6;
            this.chkTrace.Text = "Trace on";
            this.chkTrace.UseVisualStyleBackColor = true;
            // 
            // AvailableDevicesListBox
            // 
            this.AvailableDevicesListBox.FormattingEnabled = true;
            this.AvailableDevicesListBox.Location = new System.Drawing.Point(12, 27);
            this.AvailableDevicesListBox.Name = "AvailableDevicesListBox";
            this.AvailableDevicesListBox.Size = new System.Drawing.Size(184, 290);
            this.AvailableDevicesListBox.TabIndex = 7;
            // 
            // coolerGroupBox
            // 
            this.coolerGroupBox.Controls.Add(this.coolerComPortComboBox);
            this.coolerGroupBox.Controls.Add(this.coolerCheckBox);
            this.coolerGroupBox.Location = new System.Drawing.Point(210, 157);
            this.coolerGroupBox.Name = "coolerGroupBox";
            this.coolerGroupBox.Size = new System.Drawing.Size(119, 76);
            this.coolerGroupBox.TabIndex = 8;
            this.coolerGroupBox.TabStop = false;
            this.coolerGroupBox.Text = "Cooling";
            // 
            // coolerComPortComboBox
            // 
            this.coolerComPortComboBox.Enabled = false;
            this.coolerComPortComboBox.FormattingEnabled = true;
            this.coolerComPortComboBox.Location = new System.Drawing.Point(6, 42);
            this.coolerComPortComboBox.Name = "coolerComPortComboBox";
            this.coolerComPortComboBox.Size = new System.Drawing.Size(87, 21);
            this.coolerComPortComboBox.TabIndex = 10;
            // 
            // coolerCheckBox
            // 
            this.coolerCheckBox.AutoSize = true;
            this.coolerCheckBox.Location = new System.Drawing.Point(6, 19);
            this.coolerCheckBox.Name = "coolerCheckBox";
            this.coolerCheckBox.Size = new System.Drawing.Size(97, 17);
            this.coolerCheckBox.TabIndex = 9;
            this.coolerCheckBox.Text = "Cooler enabled";
            this.coolerCheckBox.UseVisualStyleBackColor = true;
            this.coolerCheckBox.CheckedChanged += new System.EventHandler(this.coolerCheckBox_CheckedChanged);
            // 
            // SetupDialogForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(343, 333);
            this.Controls.Add(this.coolerGroupBox);
            this.Controls.Add(this.AvailableDevicesListBox);
            this.Controls.Add(this.chkTrace);
            this.Controls.Add(this.picASCOM);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.cmdCancel);
            this.Controls.Add(this.cmdOK);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "SetupDialogForm";
            this.ShowIcon = false;
            this.ShowInTaskbar = false;
            this.SizeGripStyle = System.Windows.Forms.SizeGripStyle.Hide;
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "ASCOM cam8s v.0.8 driver setup";
            ((System.ComponentModel.ISupportInitialize)(this.picASCOM)).EndInit();
            this.coolerGroupBox.ResumeLayout(false);
            this.coolerGroupBox.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Button cmdOK;
        private System.Windows.Forms.Button cmdCancel;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.PictureBox picASCOM;
        private System.Windows.Forms.CheckBox chkTrace;
        private System.Windows.Forms.ListBox AvailableDevicesListBox;
        private System.Windows.Forms.GroupBox coolerGroupBox;
        private System.Windows.Forms.ComboBox coolerComPortComboBox;
        private System.Windows.Forms.CheckBox coolerCheckBox;
    }

}