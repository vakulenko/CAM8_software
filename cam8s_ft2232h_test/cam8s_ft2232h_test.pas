unit cam8s_ft2232h_test;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MYD2XX, StdCtrls;

type
  Tmain = class(TForm)
    connectBtn: TButton;
    disconnectBtn: TButton;
    WRBtn: TButton;
    readBtn: TButton;
    memo: TMemo;
    lowBtn: TButton;
    highBtn: TButton;
    procedure connectBtnClick(Sender: TObject);
    procedure disconnectBtnClick(Sender: TObject);
    procedure WRBtnClick(Sender: TObject);
    procedure readBtnClick(Sender: TObject);
    procedure lowBtnClick(Sender: TObject);
    procedure highBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  main: Tmain;

implementation

{$R *.dfm}

procedure Tmain.connectBtnClick(Sender: TObject);
begin
Open_USB_Device_By_Serial_Number(FT_CAM8A,'CAM8A');
Open_USB_Device_By_Serial_Number(FT_CAM8B,'CAM8B');
Set_USB_Device_BitMode(FT_CAM8B,$ff, $01);
Set_USB_Device_LatencyTimer(FT_CAM8B,2);
Set_USB_Device_LatencyTimer(FT_CAM8A,2);
Set_USB_Device_TimeOuts(FT_CAM8A,4000,4000);
Set_USB_Device_TimeOuts(FT_CAM8B,4000,4000);
Purge_USB_Device_In(FT_CAM8A);
Purge_USB_Device_OUT(FT_CAM8A);
Purge_USB_Device_OUT(FT_CAM8B);

connectBtn.Enabled:=false;
disconnectBtn.Enabled:=true;
lowBtn.Enabled:= true;
highBtn.Enabled:=true;
WRBtn.Enabled:=true;
readBtn.Enabled:=true;
end;

procedure Tmain.disconnectBtnClick(Sender: TObject);
begin
Close_USB_Device(FT_CAM8A);
Close_USB_Device(FT_CAM8B);
connectBtn.Enabled:=true;
disconnectBtn.Enabled:=false;
lowBtn.Enabled:= false;
highBtn.Enabled:=false;
WRBtn.Enabled:=false;
readBtn.Enabled:=false;
end;

procedure Tmain.WRBtnClick(Sender: TObject);
var x : integer;
begin
 for x := 0 to 6000 do
  if (x mod 2 = 0 ) then FT_Out_Buffer[x]:=$08
  else FT_Out_Buffer[x]:=$00;

Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,6000);
end;

procedure Tmain.readBtnClick(Sender: TObject);
begin
Get_USB_Device_Status(FT_CAM8A);
memo.Lines.add(inttostr(FT_Q_Bytes));
end;

procedure Tmain.lowBtnClick(Sender: TObject);
begin
FT_Out_Buffer[0]:=$00;
Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,1);
end;

procedure Tmain.highBtnClick(Sender: TObject);
begin
FT_Out_Buffer[0]:=$ff;
Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,1);
end;

end.
