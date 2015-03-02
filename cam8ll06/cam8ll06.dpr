// --------------------------------------------------------------------------------
// ASCOM Camera driver low-level interaction library for cam8_v0.6
// Edit Log:
// Date		    	Who	 Vers	  Description
// -----------	---	-----	---------------------------------------------------------
// 30-dec-2012	VSS	0.1 	  Creation library from grim image capture program for cam8
// 20-jan-2013  VSS 0.2     Fixed bug with vertical lines, add stop exposure functionality
// 12-mar-2013  VSS 0.3     Fixed bugs with top-frame upper horizontal line; faster ccd reading procedure, hardware based ROI
// 01-may-2013  VSS 0.4     Fastest image downloading
// 24-sep-2014  VSS 0.5     Only version number changes :)
// 10-oct-2014  VSS 0.51    Fixed with cleaning FT2232H buffer in readframe function
// 27-nov-2014  VSS 0.55    Only version number changes
// 20-feb-2015  VSS 0.6     Some code mini refactoring
// --------------------------------------------------------------------------------

library cam8ll06;
uses
  SysUtils,
  Classes,
  ExtCtrls,
  MyD2XX,
  Windows;

{$R *.res}
const
//������ �����������
cameraWidth  =3000;
//������ �����������
cameraHeight =2000;
//�������������� �������� �� ������� ����� BDBUS
portfirst = $a1;
portsecond = $c1;
xccd = 1500;
yccd = 1000;
dx = 3044-2*xccd;
dx2 = 1586-xccd;
dy = 512-(yccd div 2);
apolosa = 50;

//camera state consts
cameraIdle = 0;
cameraWaiting = 1;
cameraExposing = 2;
cameraReading = 3;
cameraDownload = 4;
cameraError = 5;

type
//driver image type
CameraImageType = array [0..CameraHeight-1,0..CameraWidth-1] of integer;

//Class for reading thread
posl = class(TThread)
  private
  //Private declarations
  protected procedure Execute; override;
end;

//GLobal variables
var
//����������-����, ���������� ��������� ���������� � �������
isConnected : boolean = false;
//����������-����, ����������, ����� ����� ���������� ����������
canStopExposureNow : boolean = true;
//��������� �������� ������ � �������� ������ FT2232HL
adress : dword;
//�������,
mBin : integer;
//����������-����, ���������� ���������� � ���������� �����
imageReady : boolean = false;
//����������-��������� ������
cameraState : integer = cameraIdle;
//������ ���������� � 15� ������
exposureTimer, Timer15V : integer;
//������ ��� �������� ����������-����� CanStopExposureCount
stopExposureTimer : integer;
//���������� �������� ����������-����� CanStopExposureCount
checkCanStopExposureCount : integer =0;
//���������� ��� ������� ������ (������ �����������)
co: posl;
//�������� ������-����������� ��� ��������
bufim:CameraImageType;
//������ ������ � ���������� �� �������
mYn,mdeltY:integer;
//������ ������ � ���������� �� ��������
mXn,mdeltX:integer;

// ��������� ��������� ������ � FT2232LH.
// ������ ������������ ����� �����:
//  1. ������� ����������� ����� � ��������� ������� (����������� ������������������ ��������� �� ������� ����� BDBUS).
//��� ���� ���������������� ��������� adress.
//  2. ����� ���� ���� ������ ���������� �� ����� ��������: n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
//������������� ���������� FT2232HL ������ ��� �������� ��� ��� �������� �� ���� ���� BDBUS. �������� 1 ����� ��� ���� �������� 65 ��.
//����� ��������� ��������� ������� n:=Write_USB_Device_Buffer(FT_CAM8B,adress) ������� �� ������������� ����������� � �� ��������������
//����. ������� ����������� ������������������ ��������� ����� ��������� ���, � �� ���������� �� �������.
//����� ���������� ����� �������� ��� ��������� (� ���� ��������� �� 24 �����!) ��� ����� ����� �������� ����� D2XX.pas, � ������ ��� MyD2XX.pas

//���������� ��������� ������ �������� ��� �������� � ���������� ����� val �� ������ adr � ���������� AD9822.
// �������� ���� � ���������������� ����.
procedure AD9822(adr:byte;val:word);
const kol = 64;
var dan:array[0..kol-1] of byte;
i:integer;
begin
  //����������� ������ �������������� ��������� �� ������� ����� BDBUS
  fillchar(dan,kol,portfirst);
  for i:=1 to 32 do
    dan[i]:=dan[i] and $fe;
  for i:=0 to 15 do
    dan[2*i+2]:=dan[2*i+2] + 2;
  if (adr and 4) = 4 then
    begin
      dan[3]:=dan[3]+4;
      dan[4]:=dan[4]+4;
    end;
  if (adr and 2) = 2 then
    begin
      dan[5]:=dan[5]+4;
      dan[6]:=dan[6]+4;
    end;
  if (adr and 1) = 1 then
    begin
      dan[7]:=dan[7]+4;
      dan[8]:=dan[8]+4;
    end;
  if (val and 256) = 256 then
    begin
      dan[15]:=dan[15]+4;
      dan[16]:=dan[16]+4;
    end;
  if (val and 128) = 128 then
    begin
      dan[17]:=dan[17]+4;
      dan[18]:=dan[18]+4;
    end;
  if (val and 64) = 64 then
    begin
      dan[19]:=dan[19]+4;
      dan[20]:=dan[20]+4;
    end;
  if (val and 32) = 32 then
    begin
      dan[21]:=dan[21]+4;
      dan[22]:=dan[22]+4;
    end;
  if (val and 16) = 16 then
    begin
      dan[23]:=dan[23]+4;
      dan[24]:=dan[24]+4;
    end;
  if (val and 8) = 8 then
    begin
      dan[25]:=dan[25]+4;
      dan[26]:=dan[26]+4;
    end;
  if (val and 4) = 4 then
    begin
      dan[27]:=dan[27]+4;
      dan[28]:=dan[28]+4;
    end;
  if (val and 2) = 2 then
    begin
      dan[29]:=dan[29]+4;
      dan[30]:=dan[30]+4;
    end;
  if (val and 1) = 1 then
    begin
      dan[31]:=dan[31]+4;
      dan[32]:=dan[32]+4;
    end;
  Write_USB_Device_Buffer(FT_CAM8B,@dan, kol);
end;

//���������� ��������� ������ �������� ��� �������� ����� val �� ������ ���������� HC595.
// �������� ���� � ���������������� ����.
procedure HC595(val:byte);
const kol = 18;
var dan:array[0..kol-1] of byte;
i:byte;
begin
  //����������� ������ �������������� ��������� �� ������� ����� BDBUS
  fillchar(dan,kol,portfirst);
  for i:=0 to 7 do
    begin
      dan[2*i+1]:=dan[2*i+1] + 2;
      if (val and $80) = $80 then
        begin
          dan[2*i]:=dan[2*i] + 4;
          dan[2*i+1]:=dan[2*i+1] + 4;
        end;
      val:=val*2;
    end;
  dan[16]:=dan[16]+ $80;
  for i:=0 to kol-1 do
    begin
      FT_Out_Buffer[2*i+adress]:=dan[i];
      FT_Out_Buffer[2*i+adress+1]:=dan[i];
    end;
  adress:=adress+2*kol;
end;

procedure shift0;
begin
  HC595($ed);
  HC595($af);
  HC595($bb);
  HC595($f9);
end;

//���������� ��������� ������ �������� ��� ������ ���� ������������� ������
procedure shift;
begin
  HC595($e9);
  HC595($ed);
  HC595($ad);
  HC595($af);
  HC595($ab);
  HC595($bb);
  HC595($b9);
  HC595($f9);
end;

//���������� ��������� ������ �������� ��� "�����" ������������ ����������� � ��������� �������
procedure shift2;
begin
  shift;
  HC595($f1);
  HC595($f1);
  HC595($f1);
  HC595($f1);
  HC595($e9);
  HC595($cd);
  HC595($cd);
  HC595($cd);
  HC595($cd);
  HC595($ed);
  HC595($af);
  HC595($ab);
  HC595($bb);
  HC595($b9);
  HC595($f9);
end;

//���������� ��������� ������ �������� ��� ������ ���� ������������� ������ + ������ SUB ��� ������ ������� �����������
procedure shift3;
begin
  HC595($e9);
  HC595($ed);
  //SUB
  HC595($ac);
  HC595($ae);
  HC595($aa);
  HC595($bb);
  HC595($b9);
  HC595($f9);
end;

//���������� ��������� ������ �������� ���:
//������� ��������������� ������. ���� ��� �� ��������,
//�� ����������� � ��� ���������� ����� ����� �������� � ������ ������ �����������
procedure clearline;
const dout : array[0..1] of byte = (portsecond,portfirst);
var x:word;
begin
  for x:=0 to 6000+192-1+200 do
    begin
      FT_Out_Buffer[adress+0]:=dout[0];
      FT_Out_Buffer[adress+1]:=dout[1];
      inc(adress,2);
    end;
end;

//���������� ��������� ������ �������� ���:
//������� ���������� ��������. ���� ��� �� ��������,
//�� ����������� � ��� ����� ����� �������� � �����������.
//��������� ���� ������� ������ � "�������" � ��������������� ��������.
//�������� ���������� ����� "������" ����������� � ��������� �������.
procedure clearframe;
var y:word;
begin
  for y:=0 to 1012-1 do
    shift;
  clearline;
end;

procedure clearline2;
const
dout : array[0..3] of byte = (portsecond,portsecond+8,portfirst+8,portfirst);
var x:integer;
begin
  for x:=0 to 79*xccd do
    begin
      FT_Out_Buffer[adress+0]:=dout[0]+$10;
      FT_Out_Buffer[adress+1]:=dout[1]+$10;
      FT_Out_Buffer[adress+2]:=dout[2]+$10;
      FT_Out_Buffer[adress+3]:=dout[3]+$10;
      inc(adress,4);
    end;
end;

//������ �������������� ���������� ������ FT2232HL � �������� ������ �����������
//��-�� ������������ AD9822 ��������� ������� ������� ����, ����� �������, � � delphi ��������.
//���������� �����  ��� integer32, � �� word16 ��-�� ������������ ��� ����������� ���������

//���������� ���� ������ ������� ����� ���� ADBUS
procedure posl.Execute;
var
x,y,x1:word;
begin
  for y:= mYn to mYn+mdeltY-1 do
    begin
      if mBin = 1 then
        begin
          Read_USB_Device_Buffer(FT_CAM8A,8*mdeltX);
          for x:=0 to mdeltX - 1 do
            begin
              x1:=x+mXn;
              bufim[2*y,2*x1]:=swap(FT_In_Buffer[4*x]);
              bufim[2*y+1,2*x1]:=swap(FT_In_Buffer[4*x+1]);
              bufim[2*y+1,2*x1+1]:=swap(FT_In_Buffer[4*x+2]);
              bufim[2*y,2*x1+1]:=swap(FT_In_Buffer[4*x+3]);
            end;
        end
      else
        begin
          Read_USB_Device_Buffer(FT_CAM8A,2*mdeltX);
          for x:=0 to mdeltX - 1 do
            begin
              x1:=x+mXn;
              bufim[2*y,2*x1]:=swap(FT_In_Buffer[x]);
              bufim[2*y,2*x1+1]:=swap(FT_In_Buffer[x]);
              bufim[2*y+1,2*x1+1]:=swap(FT_In_Buffer[x]);
              bufim[2*y+1,2*x1]:=swap(FT_In_Buffer[x]);
            end;
        end;
    end;
end;

//�������� ������ � ���������������
procedure ComRead;
begin
  co:=posl.Create(true);
  co.FreeOnTerminate:=true;
  co.Priority:=tpLower;
  co.Resume;
end;

//������������ 2 ������:
// 1.������� ��� �������.
//2.�/� � �������� 2*2.
//������������ ������� ICX453 �������� ��, ��� �������������� ������� ����� ��������� ������� �
//��� ����� ���� ������������� ������ � �������������� ������� "������" ����� ���� �����,
//������� ���������� ����� ��� ���� ���� �������� ����������.

//���������� ��������� ������ �������� � ���������� ���� �������� ������ �����
procedure readframe(bin:integer;expoz:integer);
const
dout : array[0..4] of byte = (portsecond,portsecond+8,portfirst+8,portfirst,portsecond+$28);
var x,y:integer;
begin
  cameraState := cameraReading;
  Get_USB_Device_Status(FT_CAM8A);
  if FT_Q_Bytes <> 0 then Purge_USB_Device_In(FT_CAM8A);
  Get_USB_Device_Status(FT_CAM8A);
  if FT_TxQ_Bytes <> 0 then Purge_USB_Device_Out(FT_CAM8A);
  Get_USB_Device_Status(FT_CAM8B);
  if FT_TxQ_Bytes <> 0 then Purge_USB_Device_Out(FT_CAM8B);
  adress:=0;
  if expoz > 52 then
    begin
      if expoz < 500 then
        begin
          shift3;
          for y:=0 to expoz-52 do
            for x:=0 to 416 do
              HC595($f9);
        end;
      clearline2;
      clearframe;
    end
  else
    begin
      clearline2;
      clearframe;
      shift3;
      if expoz > 0 then
        for y:=0 to expoz do
          for x:=0 to 416 do
            HC595($f9);
    end;
  shift2;
  Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
  comread;
  adress:=0;
  for y:=0 to dy-1+mYn do
    shift;
  clearline;
  for y:=0 to mdeltY -1 do
    begin
      shift;
      for x:=0 to apolosa do
        begin
          FT_Out_Buffer[adress]:=dout[3];
          inc(adress,1);
        end;
      for x:=0 to dx-1+4*mXn do
        begin
          FT_Out_Buffer[adress+0]:=dout[0];
          FT_Out_Buffer[adress+1]:=dout[3];
          inc(adress,2);
        end;
      if bin = 1 then
        begin
          for x:=0 to 3 do
            begin
              FT_Out_Buffer[adress+0]:=dout[0]+$10;
              FT_Out_Buffer[adress+1]:=dout[1]+$10;
              FT_Out_Buffer[adress+2]:=dout[2]+$10;
              FT_Out_Buffer[adress+3]:=dout[3]+$10;
              inc(adress,4);
            end;
          FT_Out_Buffer[adress+0]:=dout[0]+$10;
          FT_Out_Buffer[adress+1]:=dout[1]+$10;
          FT_Out_Buffer[adress+2]:=dout[2];
          FT_Out_Buffer[adress+3]:=dout[3];
          inc(adress,4);
          for x:=0 to 4*mdeltX-2 do
            begin
              FT_Out_Buffer[adress+0]:=dout[0];
              FT_Out_Buffer[adress+1]:=dout[1];
              FT_Out_Buffer[adress+2]:=dout[2];
              FT_Out_Buffer[adress+3]:=dout[3];
              inc(adress,4);
            end;
        end
      else
        begin
          for x:=0 to 3 do
            begin
              FT_Out_Buffer[adress+0]:=dout[0]+$10;
              FT_Out_Buffer[adress+1]:=dout[1]+$10;
              FT_Out_Buffer[adress+2]:=dout[2]+$10;
              FT_Out_Buffer[adress+3]:=dout[4]+$10;
              FT_Out_Buffer[adress+4]:=dout[2]+$10;
              FT_Out_Buffer[adress+5]:=dout[4]+$10;
              FT_Out_Buffer[adress+6]:=dout[2]+$10;
              FT_Out_Buffer[adress+7]:=dout[4]+$10;
              FT_Out_Buffer[adress+8]:=dout[2]+$10;
              FT_Out_Buffer[adress+9]:=dout[3]+$10;
              inc(adress,10);
            end;
          FT_Out_Buffer[adress+0]:=dout[0]+$10;
          FT_Out_Buffer[adress+1]:=dout[1]+$10;
          FT_Out_Buffer[adress+2]:=dout[2]+$10;
          FT_Out_Buffer[adress+3]:=dout[4]+$10;
          FT_Out_Buffer[adress+4]:=dout[2]+$10;
          FT_Out_Buffer[adress+5]:=dout[4]+$10;
          FT_Out_Buffer[adress+6]:=dout[2]+$10;
          FT_Out_Buffer[adress+7]:=dout[4]+$10;
          FT_Out_Buffer[adress+8]:=dout[2];
          FT_Out_Buffer[adress+9]:=dout[3];
          inc(adress,10);
          for x:=0 to mdeltX-2 do
            begin
              FT_Out_Buffer[adress+0]:=dout[0];
              FT_Out_Buffer[adress+1]:=dout[1];
              FT_Out_Buffer[adress+2]:=dout[2];
              FT_Out_Buffer[adress+3]:=dout[4];
              FT_Out_Buffer[adress+4]:=dout[2];
              FT_Out_Buffer[adress+5]:=dout[4];
              FT_Out_Buffer[adress+6]:=dout[2];
              FT_Out_Buffer[adress+7]:=dout[4];
              FT_Out_Buffer[adress+8]:=dout[2];
              FT_Out_Buffer[adress+9]:=dout[3];
              inc(adress,10);
            end;
        end;
      FT_Out_Buffer[adress+0]:=dout[0];
      FT_Out_Buffer[adress+1]:=dout[1];
      FT_Out_Buffer[adress+2]:=dout[2]+$10;
      FT_Out_Buffer[adress+3]:=dout[3]+$10;
      inc(adress,4);
      for x:=0 to dx2-1+6000-4*mdeltX-4*mXn do
        begin
          FT_Out_Buffer[adress+0]:=dout[0];
          FT_Out_Buffer[adress+1]:=dout[3];
          inc(adress,2);
        end;
    end;
  //������� �� �����!!
  Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
end;

//����� ���������� �����������, ������������ �������, ��������� ��������� ������ �����
procedure ExposureTimerTick; stdcall;
begin
  canStopExposureNow := false;
  KillTimer (0,ExposureTimer);
  KillTimer (0,Timer15V);
  cameraState := cameraReading;
  adress:=0;
  HC595($f9);
  Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);
  readframe (mBin, 1000);
  imageReady := true;
  cameraState:=cameraDownload;
  cameraState:=cameraIdle;
  canStopExposureNow := true;
end;

//����� ������� ����� ������ ��������� ������� +15V
procedure Timer15VTick; stdcall;
begin
  KillTimer (0,Timer15V);
  adress:=0;
  HC595($79);
  Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);
  canStopExposureNow := true;
end;

//Stop Exposure Timer, purge FTDI FT2232HL buffers and frame bufeer
procedure StopExposure;
var i,j : integer;
begin
  KillTimer (0,ExposureTimer);
  Purge_USB_Device_In(FT_CAM8A);
  Purge_USB_Device_In(FT_CAM8B);
  for i := 0 to CameraHeight-1 do
    for j := 0 to CameraWidth-1 do
      bufim[i,j] := 0;
  cameraState := cameraIdle;
  imageReady := true;
end;

//�������� ������ 0,1 ��� �� ���������� 15 ���
procedure StopExposureTimerTick; stdcall;
begin
  if (canStopExposureNow) then
    begin
      KillTimer (0,StopExposureTimer);
      StopExposure;
    end;
  checkCanStopExposureCount:=checkCanStopExposureCount+1;
  if (checkCanStopExposureCount=150) then KillTimer (0,StopExposureTimer);
end;

//Connect camera, return bool result
//����� ������������ ��������� � ������������� AD9822
function cameraConnect () : WordBool;  stdcall; export;
var  FT_OP_flag : boolean;
begin
  FT_OP_flag:=true;
  if (FT_OP_flag) then
    begin
      if Open_USB_Device_By_Serial_Number(FT_CAM8A,'CAM8A') <> FT_OK then FT_OP_flag := false;
    end;
  if (FT_OP_flag) then
    begin
      if Open_USB_Device_By_Serial_Number(FT_CAM8B,'CAM8B')  <> FT_OK then FT_OP_flag := false;
    end;
  if (FT_OP_flag) then
    begin
      // BitMode
      if Set_USB_Device_BitMode(FT_CAM8B,$ff, $01)  <> FT_OK then FT_OP_flag := false;
    end;
  if (FT_OP_flag) then
    begin
      //������������ ��������������
      Set_USB_Device_LatencyTimer(FT_CAM8B,2);
      Set_USB_Device_LatencyTimer(FT_CAM8A,2);
      Set_USB_Device_TimeOuts(FT_CAM8A,4000,4000);
      Purge_USB_Device_In(FT_CAM8A);
      Purge_USB_Device_OUT(FT_CAM8A);
      Purge_USB_Device_OUT(FT_CAM8B);
      //����� AD9822 - ����� G,2 ������ ���������, CDS �����
      AD9822(0,$58);
      //�������� �������
      AD9822(1,$a0);
      AD9822(6,14+256);
      //�������� ��������������� �����. ��� �� ������������� ���
      AD9822(3,34);
      adress:=0;
      HC595($f9);
      Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
    end;
  isConnected := FT_OP_flag;
  cameraState := cameraIdle;
  if(FT_OP_flag=false) then cameraState := cameraError;
  Result := isConnected;
end;

//Disconnect camera, return bool result
function cameraDisconnect (): WordBool; stdcall; export;
var FT_OP_flag : boolean;
begin
  FT_OP_flag := true;
  //�������� ���������
  if Close_USB_Device(FT_CAM8A) <> FT_OK then FT_OP_flag := false;
  if Close_USB_Device(FT_CAM8B) <> FT_OK then FT_OP_flag := false;
  isConnected := not FT_OP_flag;
  Result:= FT_OP_flag;
end;

//Check camera connection, return bool resul
function cameraIsConnected () : WordBool; stdcall; export;
begin
  Result := isConnected;
end;

//Set camera gain, return bool result
function cameraSetGain (val : integer) : WordBool; stdcall; export;
begin
  //�������� AD9822
  AD9822(3,val);
  Result :=true;
end;

//Set camera offset, return bool result
function cameraSetOffset (val : integer) : WordBool; stdcall; export;
var x : integer;
begin
  x:=abs(2*val);
  if val < 0 then x:=x+256;
  //�������� AD9822
  AD9822(6,x);
  Result :=true;
end;

//Make frame, return bool result
//������������ 2 �������: ExposureTimer - ��� ���������� �������� duration � �������������,
//                        Timer15V - ��� ���������� - ��������� ������� 15 V ICX453
function cameraStartExposure (Bin,StartX,StartY,NumX,NumY : integer; Duration : double; light : WordBool) : WordBool; stdcall; export;
begin
  canStopExposureNow := false;
  mbin := Bin;
  if (NumY+StartY > CameraHeight)or(StartY < 0)or(NumY <= 0) then
    begin
      mYn:=0;
      mdeltY:=yccd
    end
  else
    begin
      mYn:=StartY div 2;
      mdeltY:=NumY div 2
    end;
  if (NumX+StartX > CameraWidth)or(StartX < 0)or(NumX <= 0) then
    begin
      mXn:=0;
      mdeltX:=xccd
    end
  else
    begin
      mXn:=StartX div 2;
      mdeltX:=NumX div 2
    end;
  imageReady := false;
  //camera exposing
  cameraState := cameraExposing;
  if Duration > 0.499 then
    begin
      adress:=0;
      shift3;
      Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);
      ExposureTimer := settimer (0,0,round(Duration*1000-52),@ExposureTimerTick);
      Timer15V := settimer (0,0,1000,@Timer15VTick);
    end
  else
    begin
      cameraState := cameraReading;
      readframe (mBin,round(Duration*1000));
      imageReady := true;
      cameraState:=cameraDownload;
      cameraState:=cameraIdle;
    end;
  Result := true;
end;

//Stop camera exposure when it is possible
function cameraStopExposure : WordBool; stdcall; export;

begin
  if (canStopExposureNow) then StopExposure
  else
    begin
      checkCanStopExposureCount:=0;
      StopExposureTimer := settimer (0,0,100,@StopExposureTimerTick);
    end;
  Result := true;
end;

//Get camera state, return int result
function cameraGetCameraState : integer; stdcall; export;
begin
  Result := cameraState;
end;

//Check ImageReady flag, is image ready for transfer - transfer image to driver and return bool ImageReady flag
function cameraGetImageReady : WordBool; stdcall; export;
begin
  Result := imageReady;
end;

//Get back pointer to image
function cameraGetImage : dword; stdcall; export;
begin
  Result := dword(@bufim);
end;

exports cameraConnect;
exports cameraDisconnect;
exports cameraIsConnected;
exports cameraSetGain;
exports cameraSetOffset;
exports cameraStartExposure;
exports cameraStopExposure;
exports cameraGetCameraState;
exports cameraGetImageReady;
exports cameraGetImage;

begin
end.