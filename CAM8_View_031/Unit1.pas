unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, IniFiles, Spin, Math;//, icx453;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    Image2: TImage;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    Image3: TImage;
    Edit1: TEdit;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox2: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    CheckBox3: TCheckBox;
    Timer1: TTimer;
    CheckBox4: TCheckBox;
    Label6: TLabel;
    SpeedButton2: TSpeedButton;
    procedure Memo1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure FormMD(Sender: TObject; Button:
TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit1Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
razm = 100;                                                 //размер окна детального изображения (лупы)
xccd = 1500;
yccd = 1000;
CameraWidth  = 3000;    //ширина изображения
CameraHeight = 2000;    //высота изображения

type camera_image_type = array [0..CameraWidth-1,0..CameraHeight-1] of integer;

var
  Form1: TForm1;
  InImage:TBitmap;                                          //изображение для отображения на Image1
  InImage3:TBitmap;                                         //изображение для отображения на Image3
  gisr,gisg,gisb:array[0..255] of integer;                  //массив гистограмм
  StartXb,StartYb,SXb,SYb:integer;                          //используются для вычисления координат лупы
  ConfigFile:TIniFile;                                      //
  iso:integer;                                              //параметр выбора преобразования исходного 14- битного массива в 3*8 битный BitMap
  nframe:integer;                                           //номер кадра
  red,green,blue:integer;                                   //коэффициенты цветности RGB
  dt0,dt:TDateTime;
  buf:array [0..CameraHeight-1,0..CameraWidth-1] of word;
  ntik:integer;
  pim:pointer;
  im:camera_image_type;

implementation


{$R *.dfm}

function CameraConnect() : WordBool; stdcall; External 'icx453.dll' name 'CameraConnect';
function CameraDisConnect() : WordBool; stdcall; External 'icx453.dll' name 'CameraDisConnect';
function CameraStartExposure (Bin,StartX,StartY,NumX,NumY : integer; Duration : double; light : WordBool) : WordBool; stdcall; External 'icx453.dll' name 'CameraStartExposure';
function CameraSetGain (val : integer) : WordBool; stdcall; External 'icx453.dll' name 'CameraSetGain';
function CameraSetOffset (val : integer) : WordBool; stdcall; External 'icx453.dll' name 'CameraSetOffset';
function CameraGetImageReady (var frame : camera_image_type) : WordBool; stdcall;  External 'icx453.dll' name 'CameraGetImageReady';
function CameraGetpImageReady (var frame : dword) : WordBool; stdcall;  External 'icx453.dll' name 'CameraGetpImageReady';
function CameraGetCameraState : integer; stdcall; export; External 'icx453.dll' name 'CameraGetCameraState';

procedure display(bufi:PWordArray);//IntegerArray);
{Рисование картинки Image1 в 1 режиме}
var
x,y:word;
line : pByteArray;
bline,bliner,blineg,blineb:integer;
mash:integer;
begin
fillchar(gisr,sizeof(gisr),0);             //обнуление массива гистограммы
fillchar(gisg,sizeof(gisg),0);
fillchar(gisb,sizeof(gisb),0);
mash:=round(0.0014*yccd*xccd);
for y:=0 to yccd-1 do                         //1000 строк
  begin
  Line :=InImage.ScanLine[y];              //ScanLine используется для быстроты построения изображения
   for x:=0 to xccd-1 do
    begin
     bline:= bufi[2*y+2*x*CameraHeight];                   //первый зеленый пиксел в квартете
     blineb:=blue*bufi[(2*y+1)+2*x*CameraHeight];   //голубой пиксел, нормировка через blue
     blineg:=bline+bufi[(2*y+1)+(2*x+1)*CameraHeight];       //второй зеленый пиксел в квартете,
     blineg:=green*blineg;               //нормировка через 0.5*green
     bliner:=red*bufi[2*y+(2*x+1)*CameraHeight];      //красный пиксел, нормировка через red

     bliner:=bliner shr iso;               //сдвиг для отображения старших или младших бит
     blineg:=blineg shr (iso+1);
     blineb:=blineb shr iso;
     if (bliner > 255) then bliner:=255;   //нормировка и проведение в RGB24 (8 бит на цвет)
     if (blineg > 255) then blineg:=255;
     if (blineb > 255) then blineb:=255;
     Line^[3*x]:=blineb;                   //собственно само рисование
     Line^[3*x+1]:=blineg;
     Line^[3*x+2]:=bliner;
     inc(gisr[bliner]);                    //заполнение массива гистограммы
     inc(gisg[blineg]);
     inc(gisb[blineb]);
    end;
  end;
  SetPrecisionMode(pmExtended);
  Form1.Image1.Picture.Bitmap:=InImage;    //отображение на Image1

  With Form1 do                            //рисование гистограммы
  begin
   Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
   if CheckBox1.Checked then
   begin
   Image2.Canvas.Pen.Color:=$f0f0f0;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisr[x] div mash));
   end                  else
   begin
   Image2.Canvas.Pen.Color:=$f00000;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisb[x] div mash));
   Image2.Canvas.Pen.Color:=$00f000;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisg[x] div mash));
   Image2.Canvas.Pen.Color:=$0000f0;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisr[x] div mash));
   end;
  end;
end;

procedure display2(bufi:PWordArray);//IntegerArray);//WordArray);
{Рисование картинки Image3 в 1 режиме (лупа)}
var
x,y:word;
line,line1: pByteArray;
bline0,bline1,bline2,bline3:integer;
begin
 for y:=0 to 49 do
  begin
  Line :=InImage3.ScanLine[2*y];
  Line1 :=InImage3.ScanLine[2*y+1];
   for x:=0 to 49 do
    begin

     bline0:=bufi[(2*(y+SYb)+0)+(2*(x+SXb)+0)*CameraHeight];
     bline1:=bufi[(2*(y+SYb)+1)+(2*(x+SXb)+0)*CameraHeight];
     bline2:=bufi[(2*(y+SYb)+1)+(2*(x+SXb)+1)*CameraHeight];
     bline3:=bufi[(2*(y+SYb)+0)+(2*(x+SXb)+1)*CameraHeight];

     bline0:=green*bline0;
     bline1:=blue*bline1;
     bline2:=green*bline2;
     bline3:=red*bline3;
     bline0:=bline0 shr iso;
     bline1:=bline1 shr iso;
     bline2:=bline2 shr iso;
     bline3:=bline3 shr iso;
     if (bline0 > 255) then bline0:=255;
     if (bline1 > 255) then bline1:=255;
     if (bline2 > 255) then bline2:=255;
     if (bline3 > 255) then bline3:=255;

     Line^[6*x]:=bline1;
     Line^[6*x+1]:=bline0;
     Line^[6*x+2]:=bline3;

     Line^[6*x+3]:=bline1;
     Line^[6*x+4]:=bline0;
     Line^[6*x+5]:=bline3;

     Line1^[6*x]:=bline1;
     Line1^[6*x+1]:=bline2;
     Line1^[6*x+2]:=bline3;

     Line1^[6*x+3]:=bline1;
     Line1^[6*x+4]:=bline2;
     Line1^[6*x+5]:=bline3;
    end;
  end;
  Form1.Image3.Picture.Bitmap:=InImage3;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
 Memo1.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);                //чтение и установка первоначальных настроек
begin
   InImage:=TBitmap.Create;
   InImage.PixelFormat:=pf24bit;
   InImage.Height:=yccd;
   InImage.Width:=xccd;

   InImage3:=TBitmap.Create;
   InImage3.PixelFormat:=pf24bit;
   InImage3.Height:=razm;
   InImage3.Width:=razm;

   Image2.Canvas.Brush.Color := clBlack;
   Image2.Canvas.FillRect(Image2.Canvas.ClipRect);

   SXb:=xccd div 4;SYb:=yccd div 4;

   ConfigFile:=TIniFile.Create(GetCurrentDir+'\cam8.ini');
   if not ConfigFile.SectionExists('TIMES') then
    begin
     ConfigFile.WriteInteger('TIMES','T1',100);
     SpinEdit1.Value:=100;
    end else
    begin
     SpinEdit1.Value:=ConfigFile.ReadInteger('TIMES','T1',100);
    end;
   if not ConfigFile.SectionExists('COLORS') then
    begin
     ConfigFile.WriteInteger('COLORS','RED',0);
     TrackBar3.Position:=0;
    end else
    begin
     TrackBar3.Position:=ConfigFile.ReadInteger('COLORS','RED',0);
    end;
   if not ConfigFile.SectionExists('COLORS') then
    begin
     ConfigFile.WriteInteger('COLORS','GREEN',0);
     TrackBar4.Position:=0;
    end else
    begin
     TrackBar4.Position:=ConfigFile.ReadInteger('COLORS','GREEN',0);
    end;
   if not ConfigFile.SectionExists('COLORS') then
    begin
     ConfigFile.WriteInteger('COLORS','BLUE',0);
     TrackBar5.Position:=0;
    end else
    begin
     TrackBar5.Position:=ConfigFile.ReadInteger('COLORS','BLUE',0);
    end;
    if not ConfigFile.SectionExists('COLORS') then
    begin
     ConfigFile.WriteInteger('ISO','ISO',0);
     RadioGroup3.ItemIndex:=0;
    end else
    begin
     RadioGroup3.ItemIndex:=ConfigFile.ReadInteger('ISO','ISO',0);
    end;
    red:=round(4096*exp(0.05*TrackBar3.Position));
    green:=round(4096*exp(0.05*TrackBar4.Position));
    blue:=round(4096*exp(0.05*TrackBar5.Position));
    iso:= 20-RadioGroup3.ItemIndex;
end;

procedure TForm1.FormClose(Sender: TObject);                            //спасти настройки
begin
   ConfigFile.WriteInteger('TIMES','T1',SpinEdit1.Value);
   ConfigFile.WriteInteger('COLORS','RED',TrackBar3.Position);
   ConfigFile.WriteInteger('COLORS','GREEN',TrackBar4.Position);
   ConfigFile.WriteInteger('COLORS','BLUE',TrackBar5.Position);
   ConfigFile.WriteInteger('ISO','ISO',RadioGroup3.ItemIndex);
   InImage.Destroy;
end;

procedure TForm1.FormMD(Sender: TObject; Button:                         //вычисление участка изображения для отображения в "лупе"
TMouseButton; Shift: TShiftState; X, Y: Integer);
const
ykon = yccd - 50;
xkon = xccd - 50;
begin
  SXb := round(X*(xccd/Image1.Width)-25); //3*X-25;
  if SXb < 0 then SXb:=0;
  if SXb > xkon then SXb:=xkon;
  SYb := round(Y*(yccd/Image1.Height)-25);
  if SYb < 0 then SYb:=0;
  if SYb > ykon then SYb:=ykon;
//  Memo1.Lines.Add(inttostr(SXb)+' '+inttostr(SYb));
  if pim <> nil then display2(pim);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);                      //открытие - заккрытие устройств
begin
  if SpeedButton1.Down then
  begin
  if CameraConnect then
   begin
     SpeedButton2.Enabled:=true;
     TrackBar1.Enabled:=true;
     Trackbar2.Enabled:=true;
     Memo1.Lines.Add('connect');
   end else SpeedButton1.Down:=false;
  end else
   begin
    if Timer1.Enabled = false then
    begin
    CameraDisconnect;
    SpeedButton2.Enabled:=false;
    TrackBar1.Enabled:=false;
    Trackbar2.Enabled:=false;
    Memo1.Lines.Add('disconnect');
    end else
    begin
    SpeedButton1.Down:=true;
    Memo1.Lines.Add('экспозиция не закончена!');
    end;
   end;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);               //изменение усиления AD9822
begin
 CameraSetGain(TrackBar1.Position);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);              //изменение смещения AD9822
var
x:word;
begin
 CameraSetOffset(TrackBar2.Position);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);             //изменение режима отображения
begin
 iso:= 20-RadioGroup3.ItemIndex;
 if pim<>nil then
  begin
   display(pim);
   display2(pim);
  end; 
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);                    //хитрое изменение выдержки, мс
begin
SpinEdit1.Increment:=1;
if SpinEdit1.Value >= 10 then SpinEdit1.Increment:=2;
if SpinEdit1.Value >= 50 then SpinEdit1.Increment:=10;
if SpinEdit1.Value >= 100 then SpinEdit1.Increment:=20;
if SpinEdit1.Value >= 500 then SpinEdit1.Increment:=100;
if SpinEdit1.Value >= 1000 then SpinEdit1.Increment:=200;
if SpinEdit1.Value >= 5000 then SpinEdit1.Increment:=1000;
if SpinEdit1.Value >= 10000 then SpinEdit1.Increment:=2000;
if SpinEdit1.Value >= 50000 then SpinEdit1.Increment:=10000;
if SpinEdit1.Value >= 100000 then SpinEdit1.Increment:=20000;
if SpinEdit1.Value >= 500000 then SpinEdit1.Increment:=100000;
if SpinEdit1.Value >= 1000000 then SpinEdit1.Increment:=200000;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
 red:=round(4096*exp(0.05*TrackBar3.Position));
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
 green:=round(4096*exp(0.05*TrackBar4.Position));
end;

procedure TForm1.TrackBar5Change(Sender: TObject);
begin
 blue:=round(4096*exp(0.05*TrackBar5.Position));
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Lines.Add('время кадра = '+floattostrf(dt*86400,fffixed,6,2));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
f:TFileStream;
x,y:integer;
begin
CameraGetImageReady(im);
for y:=0 to CameraHeight-1 do for x:=0 to CameraWidth-1 do buf[y,x]:=im[x,y];
    f:=TFileStream.Create(Form1.Edit1.Text+inttostr(nframe)+'.raw',fmCreate);
    f.Write(buf,sizeof(buf));
    f.Free;
    inc(nframe);
    Form1.Memo1.Lines.Add('кадр № '+inttostr(nframe-1)+' записан');
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
 nframe:=0;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 Label6.Caption:=inttostr(ntik);dec(ntik);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
dd:double;bin:integer;
temp:dword;
begin
 if SpeedButton2.Down then
 begin
 if CheckBox1.Checked then bin:=0 else bin:=1;
 dt0:=now;dd:=SpinEdit1.Value/1000;
 if CheckBox4.Checked then CameraStartExposure(bin,2*SXb,2*SYb,100,100,dd,true)
                      else CameraStartExposure(bin,0,0,3000,2000,dd,true);
 Timer1.Enabled:=true;
 ntik:=round(dd);

 while (CameraGetpImageReady(temp)=false) do
   ;

  Timer1.Enabled:=false;
  pim:=PWord(temp);
  display(pim);
  display2(pim);
  dt:=now-dt0;
  if CheckBox3.Checked then Button3.Click;
  if CheckBox2.Checked then SpeedButton2.Click else SpeedButton2.Down:=false;
 end;
end;

end.

