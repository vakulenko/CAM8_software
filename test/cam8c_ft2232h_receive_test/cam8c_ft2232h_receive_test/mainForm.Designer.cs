namespace cam8c_ft2232h_receive_test
{
    partial class mainForm
    {
        /// <summary>
        /// Требуется переменная конструктора.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Освободить все используемые ресурсы.
        /// </summary>
        /// <param name="disposing">истинно, если управляемый ресурс должен быть удален; иначе ложно.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Код, автоматически созданный конструктором форм Windows

        /// <summary>
        /// Обязательный метод для поддержки конструктора - не изменяйте
        /// содержимое данного метода при помощи редактора кода.
        /// </summary>
        private void InitializeComponent()
        {
            this.connectBtn = new System.Windows.Forms.Button();
            this.readframeBtn = new System.Windows.Forms.Button();
            this.comPortComboBox = new System.Windows.Forms.ComboBox();
            this.listBox = new System.Windows.Forms.ListBox();
            this.SuspendLayout();
            // 
            // connectBtn
            // 
            this.connectBtn.Location = new System.Drawing.Point(130, 14);
            this.connectBtn.Name = "connectBtn";
            this.connectBtn.Size = new System.Drawing.Size(102, 23);
            this.connectBtn.TabIndex = 0;
            this.connectBtn.Text = "Connect";
            this.connectBtn.UseVisualStyleBackColor = true;
            this.connectBtn.Click += new System.EventHandler(this.connectBtn_Click);
            // 
            // readframeBtn
            // 
            this.readframeBtn.Location = new System.Drawing.Point(12, 52);
            this.readframeBtn.Name = "readframeBtn";
            this.readframeBtn.Size = new System.Drawing.Size(220, 23);
            this.readframeBtn.TabIndex = 1;
            this.readframeBtn.Text = "Read Frame";
            this.readframeBtn.UseVisualStyleBackColor = true;
            this.readframeBtn.Click += new System.EventHandler(this.readframeBtn_Click);
            // 
            // comPortComboBox
            // 
            this.comPortComboBox.FormattingEnabled = true;
            this.comPortComboBox.Location = new System.Drawing.Point(12, 16);
            this.comPortComboBox.Name = "comPortComboBox";
            this.comPortComboBox.Size = new System.Drawing.Size(100, 21);
            this.comPortComboBox.TabIndex = 4;
            // 
            // listBox
            // 
            this.listBox.FormattingEnabled = true;
            this.listBox.Location = new System.Drawing.Point(12, 83);
            this.listBox.Name = "listBox";
            this.listBox.Size = new System.Drawing.Size(220, 95);
            this.listBox.TabIndex = 5;
            // 
            // mainForm
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(254, 200);
            this.Controls.Add(this.listBox);
            this.Controls.Add(this.comPortComboBox);
            this.Controls.Add(this.readframeBtn);
            this.Controls.Add(this.connectBtn);
            this.Name = "mainForm";
            this.Text = "cam8c_test";
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Button connectBtn;
        private System.Windows.Forms.Button readframeBtn;
        private System.Windows.Forms.ComboBox comPortComboBox;
        private System.Windows.Forms.ListBox listBox;
    }
}

