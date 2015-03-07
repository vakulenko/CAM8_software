object main: Tmain
  Left = 246
  Top = 165
  Width = 217
  Height = 274
  Caption = 'CAM8S_FT2232H_TEST'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object connectBtn: TButton
    Left = 16
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Connect'
    TabOrder = 0
    OnClick = connectBtnClick
  end
  object disconnectBtn: TButton
    Left = 112
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Disconnect'
    Enabled = False
    TabOrder = 1
    OnClick = disconnectBtnClick
  end
  object WRBtn: TButton
    Left = 16
    Top = 96
    Width = 81
    Height = 25
    Caption = '3000xWR#'
    Enabled = False
    TabOrder = 2
    OnClick = WRBtnClick
  end
  object readBtn: TButton
    Left = 112
    Top = 96
    Width = 81
    Height = 25
    Caption = 'Read'
    Enabled = False
    TabOrder = 3
    OnClick = readBtnClick
  end
  object memo: TMemo
    Left = 16
    Top = 136
    Width = 177
    Height = 89
    TabOrder = 4
  end
  object lowBtn: TButton
    Left = 16
    Top = 56
    Width = 81
    Height = 25
    Caption = '0x00 to BDBUS'
    Enabled = False
    TabOrder = 5
    OnClick = lowBtnClick
  end
  object highBtn: TButton
    Left = 112
    Top = 56
    Width = 81
    Height = 25
    Caption = '0xFF to BDBUS'
    Enabled = False
    TabOrder = 6
    OnClick = highBtnClick
  end
end
