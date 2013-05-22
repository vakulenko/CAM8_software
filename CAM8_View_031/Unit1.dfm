object Form1: TForm1
  Left = 255
  Top = 194
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'CAM8 View v.0.31'
  ClientHeight = 468
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 0
    Top = 0
    Width = 500
    Height = 333
    Cursor = crCross
    Stretch = True
    OnMouseDown = FormMD
  end
  object Label1: TLabel
    Left = 8
    Top = 344
    Width = 35
    Height = 13
    Caption = #1091#1089#1080#1083#1077#1085
  end
  object Label2: TLabel
    Left = 8
    Top = 368
    Width = 41
    Height = 13
    Caption = #1089#1084#1077#1097#1077#1085
  end
  object SpeedButton1: TSpeedButton
    Left = 512
    Top = 8
    Width = 73
    Height = 33
    AllowAllUp = True
    GroupIndex = 1
    Caption = #1086#1090#1082#1088#1099#1090#1100
    OnClick = SpeedButton1Click
  end
  object Image2: TImage
    Left = 592
    Top = 0
    Width = 257
    Height = 137
  end
  object Image3: TImage
    Left = 648
    Top = 264
    Width = 200
    Height = 200
    Stretch = True
  end
  object Label3: TLabel
    Left = 8
    Top = 392
    Width = 44
    Height = 13
    Caption = #1082#1088#1072#1089#1085#1099#1081
  end
  object Label4: TLabel
    Left = 8
    Top = 416
    Width = 44
    Height = 13
    Caption = #1079#1077#1083#1077#1085#1099#1081
  end
  object Label5: TLabel
    Left = 8
    Top = 440
    Width = 30
    Height = 13
    Caption = #1089#1080#1085#1080#1081
  end
  object Label6: TLabel
    Left = 424
    Top = 440
    Width = 21
    Height = 13
    Caption = '       '
  end
  object SpeedButton2: TSpeedButton
    Left = 424
    Top = 344
    Width = 73
    Height = 41
    AllowAllUp = True
    GroupIndex = 2
    Caption = #1082#1072#1076#1088
    Enabled = False
    OnClick = SpeedButton2Click
  end
  object Memo1: TMemo
    Left = 592
    Top = 144
    Width = 257
    Height = 113
    ScrollBars = ssVertical
    TabOrder = 0
    OnDblClick = Memo1Change
  end
  object TrackBar1: TTrackBar
    Left = 56
    Top = 344
    Width = 353
    Height = 20
    Enabled = False
    Max = 63
    Position = 34
    TabOrder = 1
    ThumbLength = 10
    OnChange = TrackBar1Change
  end
  object TrackBar2: TTrackBar
    Left = 56
    Top = 368
    Width = 353
    Height = 20
    Enabled = False
    Max = 127
    Min = -127
    Position = -7
    TabOrder = 2
    ThumbLength = 10
    OnChange = TrackBar2Change
  end
  object RadioGroup3: TRadioGroup
    Left = 512
    Top = 96
    Width = 73
    Height = 153
    Caption = 'iso'
    ItemIndex = 0
    Items.Strings = (
      '15..8'
      '14..7'
      '13..6'
      '12..5'
      '11..4'
      '10..3'
      '9..2'
      '8..1'
      '7..0')
    TabOrder = 3
    OnClick = RadioGroup3Click
  end
  object CheckBox1: TCheckBox
    Left = 512
    Top = 48
    Width = 65
    Height = 17
    Caption = 'bining 2*2'
    TabOrder = 4
  end
  object SpinEdit1: TSpinEdit
    Left = 512
    Top = 72
    Width = 73
    Height = 22
    Increment = 5
    MaxValue = 2000000
    MinValue = 0
    TabOrder = 5
    Value = 20
    OnChange = SpinEdit1Change
  end
  object Edit1: TEdit
    Left = 512
    Top = 288
    Width = 121
    Height = 21
    TabOrder = 6
    Text = 'Noname'
    OnChange = Edit1Change
  end
  object TrackBar3: TTrackBar
    Left = 56
    Top = 392
    Width = 233
    Height = 20
    Max = 25
    Min = -5
    TabOrder = 7
    ThumbLength = 10
    OnChange = TrackBar3Change
  end
  object TrackBar4: TTrackBar
    Left = 56
    Top = 416
    Width = 233
    Height = 20
    Max = 25
    Min = -5
    TabOrder = 8
    ThumbLength = 10
    OnChange = TrackBar4Change
  end
  object TrackBar5: TTrackBar
    Left = 56
    Top = 440
    Width = 233
    Height = 20
    Max = 25
    Min = -5
    TabOrder = 9
    ThumbLength = 10
    OnChange = TrackBar5Change
  end
  object CheckBox2: TCheckBox
    Left = 512
    Top = 344
    Width = 57
    Height = 17
    Caption = #1086#1087#1088#1086#1089
    TabOrder = 10
  end
  object Button2: TButton
    Left = 424
    Top = 400
    Width = 75
    Height = 25
    Caption = #1089#1090#1072#1090#1080#1089#1090#1080#1082#1072
    TabOrder = 11
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 512
    Top = 256
    Width = 57
    Height = 25
    Caption = #1092#1072#1081#1083
    TabOrder = 12
    OnClick = Button3Click
  end
  object CheckBox3: TCheckBox
    Left = 576
    Top = 264
    Width = 65
    Height = 17
    Caption = #1072#1074#1090#1086#1079#1072#1087
    TabOrder = 13
  end
  object CheckBox4: TCheckBox
    Left = 512
    Top = 368
    Width = 57
    Height = 17
    Caption = #1086#1082#1085#1086
    TabOrder = 14
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 616
    Top = 65528
  end
end
