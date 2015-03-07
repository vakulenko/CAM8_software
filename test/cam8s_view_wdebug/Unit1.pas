unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons, IniFiles, Spin, Math;

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
    CheckBox5: TCheckBox;
    SpeedButton3: TSpeedButton;
    Button1: TButton;
    Button4: TButton;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    CheckBox8: TCheckBox;
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
    procedure MessageReceiver(var msg: TMessage); message 2048;
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure fin;
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
mporog = 0.005*CameraWidth*CameraHeight;

type camera_image_type = array [0..CameraWidth-1,0..CameraHeight-1] of integer;

var
  Form1: TForm1;
  InImage:TBitmap;                                          //изображение для отображения на Image1
  InImage3:TBitmap;                                         //изображение для отображения на Image3
  gisr,gisg,gisb:array[0..255] of integer;                  //массив гистограмм
  gis:array [0..65535] of integer;
  migis,magis,degis:integer;
  StartXb,StartYb,SXb,SYb:integer;                          //используются для вычисления координат лупы
  ConfigFile:TIniFile;                                      //
  iso:integer;                                              //параметр выбора преобразования исходного 14- битного массива в 3*8 битный BitMap
  nframe:integer;                                           //номер кадра
  red,green,blue,red1,green1,blue1:integer;                                   //коэффициенты цветности RGB
  dt0,dt:TDateTime;
  buf:array [0..CameraHeight-1,0..CameraWidth-1] of word;
  ntik:integer;
  pim:pointer;
  im:camera_image_type;
  tb2p,nbias,sbias:integer;
  biasbuf,biasbuf2:array [0..CameraHeight-1,0..CameraWidth-1] of integer;
  fbias:file;
  erms:boolean;

implementation


{$R *.dfm}

function CameraConnect() : WordBool; stdcall; External 'icx453.dll' name 'CameraConnect';
function CameraDisConnect() : WordBool; stdcall; External 'icx453.dll' name 'CameraDisConnect';
function CameraStartExposure (Bin,StartX,StartY,NumX,NumY : integer; Duration : double; light : WordBool) : WordBool; stdcall; External 'icx453.dll' name 'CameraStartExposure';
function CameraSetGain (val : integer) : WordBool; stdcall; External 'icx453.dll' name 'CameraSetGain';
function CameraSetOffset (val : integer) : WordBool; stdcall; External 'icx453.dll' name 'CameraSetOffset';
function CameraGetImageReady (var frame : camera_image_type) : WordBool; stdcall;  External 'icx453.dll' name 'CameraGetImageReady';
function CameraGetCameraState : integer; stdcall; export; External 'icx453.dll' name 'CameraGetCameraState';

procedure display(bufi:PWordArray);//IntegerArray);
{Рисование картинки Image1 в 1 режиме}
var
x,y:word;
line : pByteArray;
bline,bliner,blineg,blineb:integer;
mash:integer;
procedure soldgis;
var x,y,kol,kol2:integer;
begin
 fillchar(gis,sizeof(gis),0);
 migis:=0;magis:=0;
 for y:=0 to CameraHeight-1 do
    for x:=0 to CameraWidth-1 do
      begin
       inc(gis[bufi[y+x*CameraHeight]-biasbuf[y,x]]);
      end;
     kol:=0;kol2:=0;
     for x:=0 to 65535 do
      begin
       kol:=kol+gis[x];
       kol2:=kol2+gis[65535-x];
       if kol < mporog then migis:=x;
       if kol2 < mporog then magis:=65535-x;
      end; 
      if migis = magis then magis:=migis+1;
      degis:=magis-migis;
end;

begin
fillchar(gisr,sizeof(gisr),0);             //обнуление массива гистограммы
fillchar(gisg,sizeof(gisg),0);
fillchar(gisb,sizeof(gisb),0);
if Form1.CheckBox8.Checked then soldgis;

mash:=round(0.0005*yccd*xccd);
for y:=0 to yccd-1 do                         //1000 строк
  begin
  Line :=InImage.ScanLine[y];              //ScanLine используется для быстроты построения изображения
   for x:=0 to xccd-1 do
    begin
     bline:= bufi[2*y+2*x*CameraHeight]-biasbuf[2*y,2*x];                   //первый зеленый пиксел в квартете
     if bline < 0 then bline:=0;
     buf[2*y,2*x]:=bline;

     blineb:=bufi[(2*y+1)+2*x*CameraHeight]-biasbuf[2*y+1,2*x];   //голубой пиксел, нормировка через blue
     if blineb < 0 then blineb:=0;
     buf[2*y+1,2*x]:=blineb;

     blineg:=bufi[(2*y+1)+(2*x+1)*CameraHeight]-biasbuf[2*y+1,2*x+1];       //второй зеленый пиксел в квартете,
     if blineg < 0 then blineg:=0;
     buf[2*y+1,2*x+1]:=blineg;
     blineg:=blineg+bline;

     bliner:=bufi[2*y+(2*x+1)*CameraHeight]-biasbuf[2*y,2*x+1];      //красный пиксел, нормировка через red
     if bliner < 0 then bliner:=0;
     buf[2*y,2*x+1]:=bliner;

     if Form1.CheckBox8.Checked then
      begin
       bliner:=bliner-migis;
       if bliner < 0 then bliner:=0;
       bliner:=red*bliner shr 4;
       bliner:=bliner div degis;
       blineg:=blineg-migis-migis;
       if blineg < 0 then blineg:=0;
       blineg:=green*blineg shr 5;
       blineg:=blineg div degis;
       blineb:=blineb-migis;
       if blineb < 0 then blineb:=0;
       blineb:=blue*blineb shr 4;
       blineb:=blineb div degis;
      end  else
      begin
       bliner:=red*bliner shr iso;               //сдвиг для отображения старших или младших бит
       blineg:=green*blineg shr (iso+1);
       blineb:=blue*blineb shr iso;
      end;
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

     bline0:=bufi[(2*(y+SYb)+0)+(2*(x+SXb)+0)*CameraHeight]-biasbuf[2*(y+SYb)+0,2*(x+SXb)+0];
     if bline0 < 0 then bline0:=0;
     bline1:=bufi[(2*(y+SYb)+1)+(2*(x+SXb)+0)*CameraHeight]-biasbuf[2*(y+SYb)+1,2*(x+SXb)+0];
     if bline1 < 1 then bline1:=1;
     bline2:=bufi[(2*(y+SYb)+1)+(2*(x+SXb)+1)*CameraHeight]-biasbuf[2*(y+SYb)+1,2*(x+SXb)+1];
     if bline2 < 0 then bline2:=0;
     bline3:=bufi[(2*(y+SYb)+0)+(2*(x+SXb)+1)*CameraHeight]-biasbuf[2*(y+SYb)+0,2*(x+SXb)+1];
     if bline3 < 0 then bline3:=0;

     if Form1.CheckBox8.Checked then
      begin
       bline0:=bline0-migis;
       if bline0 < 0 then bline0:=0;
       bline0:=green*bline0 shr 4;
       bline0:=bline0 div degis;

       bline1:=bline1-migis;
       if bline1 < 0 then bline1:=0;
       bline1:=blue*bline1 shr 4;
       bline1:=bline1 div degis;

       bline2:=bline2-migis;
       if bline2 < 0 then bline2:=0;
       bline2:=green*bline2 shr 4;
       bline2:=bline2 div degis;

       bline3:=bline3-migis;
       if bline3 < 0 then bline3:=0;
       bline3:=red*bline3 shr 4;
       bline3:=bline3 div degis;

      end  else
      begin

     bline0:=green*bline0;
     bline1:=blue*bline1;
     bline2:=green*bline2;
     bline3:=red*bline3;
     bline0:=bline0 shr iso;
     bline1:=bline1 shr iso;
     bline2:=bline2 shr iso;
     bline3:=bline3 shr iso;
     end;
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
    if not ConfigFile.SectionExists('BIAS') then
    begin
     ConfigFile.WriteInteger('BIAS','BIAS',0);
     TB2P:=0;
    end else
    begin
     TB2P:=ConfigFile.ReadInteger('BIAS','BIAS',0);
    end;
    if not ConfigFile.SectionExists('BIAS') then
    begin
     ConfigFile.WriteInteger('BIAS','SBIAS',100);
     sbias:=100;
    end else
    begin
     sbias:=ConfigFile.ReadInteger('BIAS','SBIAS',100);
    end;
    red:=round(4096*exp(0.06*TrackBar3.Position));
    green:=round(4096*exp(0.06*TrackBar4.Position));
    blue:=round(4096*exp(0.06*TrackBar5.Position));
    iso:= 20-RadioGroup3.ItemIndex;
end;

procedure TForm1.FormClose(Sender: TObject);                            //спасти настройки
begin
   ConfigFile.WriteInteger('TIMES','T1',SpinEdit1.Value);
   ConfigFile.WriteInteger('COLORS','RED',TrackBar3.Position);
   ConfigFile.WriteInteger('COLORS','GREEN',TrackBar4.Position);
   ConfigFile.WriteInteger('COLORS','BLUE',TrackBar5.Position);
   ConfigFile.WriteInteger('ISO','ISO',RadioGroup3.ItemIndex);
   ConfigFile.WriteInteger('BIAS','BIAS',TrackBar2.Position);
   ConfigFile.WriteInteger('BIAS','SBIAS',sbias);
   
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
     SpeedButton3.Enabled:=true;
     TrackBar1.Enabled:=true;
     Trackbar2.Enabled:=true;
     Trackbar2.Position:=TB2P;
     Memo1.Lines.Add('connect');
     CameraSetOffset(TrackBar2.Position);
   end else SpeedButton1.Down:=false;
  end else
   begin
    if Timer1.Enabled = false then
    begin
    CameraDisconnect;
    SpeedButton2.Enabled:=false;
    SpeedButton3.Enabled:=false;
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
 Label7.Caption:=inttostr(TrackBar1.Position);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);              //изменение смещения AD9822
var
x:word;
begin
 CameraSetOffset(TrackBar2.Position);
 Label8.Caption:=inttostr(TrackBar2.Position);
 ConfigFile.WriteInteger('BIAS','BIAS',TrackBar2.Position);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);             //изменение режима отображения
begin
 iso:= 20-RadioGroup3.ItemIndex;
 if pim <> nil then
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
var
max,min:integer;
x,y:integer;
sr,sq,srr,srg,srb:real;
begin
  min:=65535;max:=0;
  sr:=0;
  for y:=0 to CameraHeight-1 do
    for x:=0 to CameraWidth-1 do
     begin
      if buf[y,x] < min then min:=buf[y,x];
      if buf[y,x] > max then max:=buf[y,x];
      sr:=sr+buf[y,x];
     end;
   sr:=sr/(CameraHeight*CameraWidth);
   sq:=0;
   for y:=0 to CameraHeight-1 do
    for x:=0 to CameraWidth-1 do
     begin
      sq:=sq+(buf[y,x]-sr)*(buf[y,x]-sr);
     end;
//   sq:=sqrt(sq);  
   sq:=sqrt(sq/(CameraHeight*CameraWidth));
  srr:=0;srg:=0;srb:=0;
  for y:=0 to yccd-1 do
    for x:=0 to xccd-1 do
     begin
      srg:=srg+buf[2*y,2*x];
      srb:=srb+buf[2*y+1,2*x];
      srg:=srg+buf[2*y+1,2*x+1];
      srr:=srr+buf[2*y,2*x+1];
     end;
  srr:=srr/(yccd*xccd);srg:=srg/(2*yccd*xccd);srb:=srb/(yccd*xccd);
  Memo1.Lines.Add('время кадра = '+floattostrf(dt*86400,fffixed,6,2));
  Memo1.Lines.Add('min = '+inttostr(min)+' max = '+inttostr(max));
  Memo1.Lines.Add('сред = '+inttostr(round(sr)));
  Memo1.Lines.Add('сред кв = '+floattostrf(sq,fffixed,6,1));
  Memo1.Lines.Add('сред red = '+inttostr(round(srr)));
  Memo1.Lines.Add('сред green = '+inttostr(round(srg)));
  Memo1.Lines.Add('сред blu = '+inttostr(round(srb)));
 { Memo1.Lines.Add('0,5% гистограммы = '+inttostr(migis));
  Memo1.Lines.Add('99,5% гистограммы = '+inttostr(magis));
  Memo1.Lines.Add('делта 99% гистограммы = '+inttostr(magis-migis));
  Memo1.Lines.Add('');                     }
end;

procedure TForm1.Button3Click(Sender: TObject);
var
f:TFileStream;
x,y:integer;
begin
//CameraGetImageReady(im);
//for y:=0 to CameraHeight-1 do for x:=0 to CameraWidth-1 do buf[y,x]:=im[x,y];

    if CheckBox5.Checked then
    begin
    InImage.SaveToFile(Form1.Edit1.Text+inttostr(nframe)+'.bmp');
    end else
    begin
    f:=TFileStream.Create(Form1.Edit1.Text+inttostr(nframe)+'.raw',fmCreate);
    f.Write(buf,sizeof(buf));
    f.Free;
    end;
    inc(nframe);
    Form1.Memo1.Lines.Add('кадр № '+inttostr(nframe-1)+' записан');
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
 nframe:=0;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 if ntik > 0 then dec(ntik)
 else
 begin
  Timer1.Enabled:=false;
  if erms then fin;
 end;
 Label6.Caption:=inttostr(ntik);
end;

procedure TForm1.fin;
var
x,y:integer;
begin
  display(pim);
  display2(pim);

  dt:=now-dt0;
  if CheckBox3.Checked then Button3.Click;
  if CheckBox2.Checked then SpeedButton2.Click else SpeedButton2.Down:=false;
  if SpeedButton3.Down then
   begin
    for y:=0 to CameraHeight-1 do
    for x:=0 to CameraWidth-1 do
     biasbuf2[y,x]:=biasbuf2[y,x]+buf[y,x];
    inc(nbias);
    Memo1.Lines.Add('кадр биас/дарк № '+inttostr(nbias));
   end;
end;

procedure TForm1.MessageReceiver(var msg: TMessage);
begin
  pim:=PWord(msg.lParam);
  msg.Result := 1;
  fin;
  erms:=false;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
dd:double;bin:integer;
zat:boolean;
begin
 zat:=CheckBox7.Checked;
 erms:=true;
 if SpeedButton2.Down then
 begin
 if CheckBox1.Checked then bin:=0 else bin:=1;
 dt0:=now;
 dd:=SpinEdit1.Value/1000;
 if SpeedButton3.Down then if CheckBox6.Checked then dd:=0;
 if CheckBox4.Checked then CameraStartExposure(bin,2*SXb,2*SYb,100,100,dd,zat)
                      else CameraStartExposure(bin,0,0,3000,2000,dd,zat);
 Timer1.Enabled:=true;
 ntik:=round(dd);
 end;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var
x,y:integer;
begin
 if SpeedButton3.Down then
  begin
   nbias:=0;
   fillchar(biasbuf2,sizeof(biasbuf2),0);
   fillchar(biasbuf,sizeof(biasbuf),0);
   assignfile(fbias,'bias.raw');
   rewrite(fbias,sizeof(biasbuf));
  end                 else
  begin
   if nbias <> 0 then
   for y:=0 to CameraHeight-1 do
    for x:=0 to CameraWidth-1 do
     biasbuf[y,x]:=-sbias+biasbuf2[y,x] div nbias;
     blockwrite(fbias,biasbuf,1);
     closefile(fbias);
     Memo1.Lines.Add('дарк/биас готов');
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 assignfile(fbias,'bias.raw');
 reset(fbias,sizeof(biasbuf));
 blockread(fbias,biasbuf,1);
 closefile(fbias);
 Memo1.Lines.Add('загрузка Ok!');
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 fillchar(biasbuf,sizeof(biasbuf),0);
 Memo1.Lines.Add('очистка Ok!');
end;

end.

