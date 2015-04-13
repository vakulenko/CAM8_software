unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, MyD2XX, ComCtrls, Buttons, IniFiles, Spin, Math, SyncObjs;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Timer1: TTimer;
    Image1: TImage;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    Image2: TImage;
    Button1: TButton;
    RadioGroup3: TRadioGroup;
    CheckBox1: TCheckBox;
    SpinEdit1: TSpinEdit;
    Timer2: TTimer;
    Image3: TImage;
    SpeedButton3: TSpeedButton;
    Edit1: TEdit;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    CheckBox2: TCheckBox;
    SpinEdit2: TSpinEdit;
    Button2: TButton;
    Button3: TButton;
    CheckBox3: TCheckBox;
    procedure Memo1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure TrackBar2Change(Sender: TObject);
    procedure RadioGroup3Click(Sender: TObject);
    procedure FormMD(Sender: TObject; Button:
TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpinEdit1Change(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    procedure TrackBar4Change(Sender: TObject);
    procedure TrackBar5Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type
  posl = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  end;

const
xccd = 1500;                                                //ширина изображения для отображения на Image1 (рав изображение в 2 раза шире)
yccd = 1000;                                                //высота изображения для отображения на Image1 (рав изображение в 2 раза выше)
razm = 100;                                                 //размер окна детального изображения (лупы)
portfirst = $a1;                                            //первоначальное значение на выводах порта BDBUS
portsecond = $c1;                                           //portfirst+$40 - $20;
dx = 3044-2*xccd;
dx2 = 1586-xccd;
dy = 512-(yccd div 2);

var
  Form1: TForm1;
  InImage:TBitmap;                                          //изображение для отображения на Image1
  InImage3:TBitmap;                                         //изображение для отображения на Image3
  gis,gisr,gisg,gisb:array[0..255] of dword;                //массив гистограмм
  bufim:array[0..4*xccd,0..yccd] of integer;                //буферный массив-изображение для операций
  bias:array[0..4*xccd,0..yccd] of word;                    //массив-изображение биас
  StartXb,StartYb,SXb,SYb:integer;                          //используются для вычисления координат лупы
  ConfigFile:TIniFile;                                      //
  co: posl;                                                 //переменная для второго потока (чтение изображения)
  n:word;                                                   //
  nnn:word;                                                 //
  adress:dword;                                             //указатель текущего адреса в выходном буфере FT2232HL
  iso:word;                                                 //параметр выбора преобразования исходного 14- битного массива в 3*8 битный BitMap
  kolbyte:dword;                                            //
  nframe:integer;                                           //номер кадра
  red,green,blue:single;                                    //коэффициенты цветности RGB
  zap:boolean;                                              //
  dt0,dt:TDateTime;
  hev:TEvent;

implementation


{$R *.dfm}

procedure posl.Execute;                                     //собственно само чтение массива через порт ADBUS
{ Хитрое преобразование считанного буфера FT2232HL в буферный массив изображения
  из-за особенностей AD9822 считываем сначала старший байт, потом младший, а в delphi наоборот.
  Используем также  тип integer32, а не word16 из-за переполнения при последующих операциях }
var
x,y:word;
kol:dword;
begin
 kol:=kolbyte div (2*yccd);
 for y:= 0 to yccd-1 do
  begin
   Read_USB_Device_Buffer(FT_CAM8A,2*kol);
   for x:=0 to kol - 2 do bufim[x,y]:=FT_In_Buffer[2*x+2]+256*FT_In_Buffer[2*x+1];
   bufim[kol-1,y]:=256*FT_In_Buffer[2*kol-1];
  end;
 hev.SetEvent; 
end;

function ComRead:integer;                                   //создание потока с самоликвидацией
begin
  co:=posl.Create(true);
  co.FreeOnTerminate:=true;
  co.Priority:=tpLowest;//r;//Normal;
  co.Resume;
end;

procedure preobr;
{ Здесь вычитаем массив bias, который при отжатом Speedbutton3 (bias) равен нулю.
  Добавляем небольшое смещение sm = 400 для небольшого поднятия яркости.
  Нормируем в пределах 0..65535 буферный массив изображения bufim.
  Также пишем raw-файл, если нажат Speedbutton4 (файл). }
label 1;
const
sm =100;  //400;
var
x,y:word;
kol:dword;
begin
kol:=kolbyte div (2*yccd);
if Form1.SpeedButton3.Down = true then
   begin
     for y:=0 to yccd - 1 do for x:=0 to kol - 1 do
      begin
       bufim[x,y]:=bufim[x,y]-bias[x,y]+sm;
       if bufim[x,y] < 0 then bufim[x,y]:=0;
      end;
   end;
end;



{ Небольшое пояснение работы с FT2232LH.
 Всегда используется такой прием:
  1. Вначале заполняется буфер и исходными байтами (необходимой последовательности импульсов на выводах порта BDBUS).
При этом инкрементируется указатель adress.
  2. Далее весь этот массив передается на выход командой: n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
Замечательная микросхема FT2232HL честно без задержек все это передает на свой порт BDBUS. Передача 1 байта при этом занимает 65 нс.
Время отработки следующей команды n:=Write_USB_Device_Buffer(FT_CAM8B,adress) зависит от загруженности операционки и не контролируется
нами. Поэтому критическую последовательности импульсов нужно заполнять всю, а не передавать по очереди.
Благо програмный буфер драйвера это позволяет (в этой программе до 24 Мбайт!) Для этого нужно изменить текст D2XX.pas, я назвал его MyD2XX.pas}


procedure AD9822(adr:byte;val:word);
{Заполнение выходного буфера массивом для передачи и размещения байта val по адресу adr в микросхеме AD9822.
 Передача идет в последовательном коде.}
const
kol = 64;
var
dan:array[0..kol-1] of byte;
i:integer;
begin
 fillchar(dan,kol,portfirst);                                   //заполняется массив первоначальным значением на выводах порта BDBUS
 for i:=1 to 32 do dan[i]:=dan[i] and $fe;
 for i:=0 to 15 do dan[2*i+2]:=dan[2*i+2] + 2;
 if (adr and 4) = 4 then begin dan[3]:=dan[3]+4;dan[4]:=dan[4]+4;end;
 if (adr and 2) = 2 then begin dan[5]:=dan[5]+4;dan[6]:=dan[6]+4;end;
 if (adr and 1) = 1 then begin dan[7]:=dan[7]+4;dan[8]:=dan[8]+4;end;

 if (val and 256) = 256 then begin dan[15]:=dan[15]+4;dan[16]:=dan[16]+4;end;
 if (val and 128) = 128 then begin dan[17]:=dan[17]+4;dan[18]:=dan[18]+4;end;
 if (val and 64) = 64 then begin dan[19]:=dan[19]+4;dan[20]:=dan[20]+4;end;
 if (val and 32) = 32 then begin dan[21]:=dan[21]+4;dan[22]:=dan[22]+4;end;
 if (val and 16) = 16 then begin dan[23]:=dan[23]+4;dan[24]:=dan[24]+4;end;
 if (val and 8) = 8 then begin dan[25]:=dan[25]+4;dan[26]:=dan[26]+4;end;
 if (val and 4) = 4 then begin dan[27]:=dan[27]+4;dan[28]:=dan[28]+4;end;
 if (val and 2) = 2 then begin dan[29]:=dan[29]+4;dan[30]:=dan[30]+4;end;
 if (val and 1) = 1 then begin dan[31]:=dan[31]+4;dan[32]:=dan[32]+4;end;

 for i:=0 to kol-1 do FT_Out_Buffer[i+adress]:=dan[i];
 adress:=adress+kol;
end;

procedure HC595(val:byte);
{Заполнение выходного буфера массивом для передачи байта val на выводы микросхемы HC595.
 Передача идет в последовательном коде.}
const
kol = 18;
var
dan:array[0..kol-1] of byte;
i:integer;
begin
 fillchar(dan,kol,portfirst);                                    //заполняется массив первоначальным значением на выводах порта BDBUS
 for i:=0 to 7 do
  begin
   dan[2*i+1]:=dan[2*i+1] + 2;
   if (val and $80) = $80 then begin dan[2*i]:=dan[2*i] + 4;dan[2*i+1]:=dan[2*i+1] + 4; end;
   val:=val*2;
  end;
   dan[16]:=dan[16]+ $80;
//   dan[17]:=dan[17]+ $80;

 for i:=0 to kol-1 do
  begin
   FT_Out_Buffer[2*i+adress]:=dan[i];
   FT_Out_Buffer[2*i+adress+1]:=dan[i];
  end;
 adress:=adress+2*kol;
end;

procedure shift;
{Заполнение выходного буфера массивом для одного шага вертикального сдвига}
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

procedure shift2;
{Заполнение выходного буфера массивом для "слива" накопленного изображения в сдвиговый регистр}
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
 HC595($af);//79);
 HC595($ab);//69);
 HC595($bb);//6d);
 HC595($b9);//2d);
 HC595($f9);//2f);
end;

procedure shift3;
{Заполнение выходного буфера массивом для одного шага вертикального сдвига + сигнал SUB для очитки области изображения}
begin
 HC595($e9);
 HC595($ed);
 HC595($ac);//2d);SUB
 HC595($ae);//2f);
 HC595($aa);//2b);
 HC595($bb);
 HC595($b9);
 HC595($f9);
end;

procedure clearline;
{Заполнение выходного буфера массивом для:
 Очистка горизонтального сдвига. Если его не очистить,
 то накопленный в нем паразитный заряд будет добавлен в первую строку изображения}
const
dout : array[0..1] of byte = (portsecond,portfirst);
var
x:word;
begin
 for x:=0 to 6000+192-1 do
 begin
  FT_Out_Buffer[adress+0]:=dout[0];
  FT_Out_Buffer[adress+1]:=dout[1];
  inc(adress,2);
 end;
end;

procedure clearframe;
{Заполнение выходного буфера массивом для:
 Очистка сдвигового регистра. Если его не очистить,
 то накопленный в нем заряд будет добавлен в изображение.
 Очищается весь регистр вместе с "темными" и неиспользуемыми строками. 
 Операция проводится перед "сливом" изображения в сдвиговый регистр.}
var
y:word;
begin
 for y:=0 to 1012-1 do shift;
 clearline;
end;

{Используется 2 режима:
 1.Цветной без бининга.
 2.Ч/Б с бинингом 2*2.
 Особенностью матрицы ICX453 является то, что горизонтальный регистр имеет удвоенную емкость и
 при одном шаге вертикального сдвига в горизонтальный регистр "падает" сразу пара строк,
 поэтому количество строк для этих двух режимиов одинаковое.
 Соответствия процедур:
  readframe, display, display2 - для 1 режима,
  readframe2, display3, display4 - для 2 режима}

procedure readframe(load:boolean);
{Заполнение выходного буфера массивом и собственно сама операция чтения кадра в 1 режиме}
const
dout : array[0..3] of byte = (portsecond,portsecond+8,portfirst+8,portfirst);
var x,y:word;
begin
adress:=0;
clearframe;
if load then shift2;                  //если load, то изображение "сливается" в сдвиговый регистр, при not load формируется bias
n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
kolbyte:=8*yccd*xccd;
 comread;
adress:=0;
for y:=0 to dy-1 do shift;
clearline;
for y:=0 to yccd-1 do
begin
shift;
for x:=0 to dx-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;
for x:=0 to 3 do                         // особенность AD9822 первые 4 пиксела "пусты", просто заглушаем импульс записи в порт ADBUS
begin
 FT_Out_Buffer[adress+0]:=dout[0]+$10;
 FT_Out_Buffer[adress+1]:=dout[1]+$10;
 FT_Out_Buffer[adress+2]:=dout[2]+$10;
 FT_Out_Buffer[adress+3]:=dout[3]+$10;
 inc(adress,4);
end;
for x:=0 to 4*xccd-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[1];
 FT_Out_Buffer[adress+2]:=dout[2];
 FT_Out_Buffer[adress+3]:=dout[3];
 inc(adress,4);
end;
for x:=0 to dx2-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;
end;
 n:=Write_USB_Device_Buffer(FT_CAM8B,adress);   //команда на вывод!!
end;

procedure readframe2(load:boolean);
{Заполнение выходного буфера массивом и собственно сама операция чтения кадра во 2 режиме}
const
dout : array[0..4] of byte = (portsecond,portsecond+8,portfirst+8,portfirst,portsecond+$28);
var x,y:word;
begin
adress:=0;
clearframe;
if load then shift2;                               //если load, то изображение "сливается" в сдвиговый регистр, при not load формируется bias
n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
kolbyte:=2*xccd*yccd;
comread;
adress:=0;
for y:=0 to dy-1 do shift;
clearline;
for y:=0 to yccd-1 do
begin
shift;
for x:=0 to dx-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;
for x:=0 to 3 do                                             // особенность AD9822 первые 4 пиксела "пусты", просто заглушаем импульс записи в порт ADBUS
begin
 FT_Out_Buffer[adress+0]:=dout[0]+$10;
 FT_Out_Buffer[adress+1]:=dout[1]+$10;
 FT_Out_Buffer[adress+2]:=dout[2]+$10;
 FT_Out_Buffer[adress+3]:=dout[3]+$10;
 inc(adress,4);
end;
for x:=0 to xccd-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];                           //собственно сам бининг, есть сигнал сброса RS только для каждого 4 - го пикселя
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
for x:=0 to dx2-1 do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;
end;
 n:=Write_USB_Device_Buffer(FT_CAM8B,adress);          //команда на вывод!!
end;

procedure display(f:boolean);
{Рисование картинки Image1 в 1 режиме}
var
x,y:word;
line : pByteArray;
bline,bliner,blineg,blineb:longint;
s:single;
mash:integer;
begin
fillchar(gisr,sizeof(gisr),0);             //обнуление массива гистограммы
fillchar(gisg,sizeof(gisg),0);
fillchar(gisb,sizeof(gisb),0);
mash:=round(0.0014*yccd*xccd);
if f then preobr;
for y:=0 to yccd-1 do                         //1000 строк
  begin
  Line :=InImage.ScanLine[y];              //ScanLine используется для быстроты построения изображения
   for x:=0 to xccd-1 do
    begin
     bline:= bufim[4*x,y];                 //первый зеленый пиксел в квартете
     blineb:=round(blue*bufim[4*x+1,y]);   //голубой пиксел, нормировка через blue
     blineg:=bline+bufim[4*x+2,y];         //второй зеленый пиксел в квартете,
     blineg:=round(0.5*green*blineg);      //нормировка через 0.5*green
     bliner:=round(red*bufim[4*x+3,y]);    //красный пиксел, нормировка через red
     bliner:=bliner shr iso;               //сдвиг для отображения старших или младших бит
     blineg:=blineg shr iso;
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
   Image2.Canvas.Pen.Color:=$f00000;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisb[x] div mash));
   Image2.Canvas.Pen.Color:=$00f000;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisg[x] div mash));
   Image2.Canvas.Pen.Color:=$0000f0;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gisr[x] div mash));
  end;
end;

procedure display2;
{Рисование картинки Image3 в 1 режиме (лупа)}
var
x,y:word;
line,line1: pByteArray;
bline0,bline1,bline2,bline3:longint;
begin
 for y:=0 to 49 do
  begin
  Line :=InImage3.ScanLine[2*y];
  Line1 :=InImage3.ScanLine[2*y+1];
   for x:=0 to 49 do
    begin
     bline0:=bufim[4*(x+SXb)+0,y+SYb];
     bline1:=bufim[4*(x+SXb)+1,y+SYb];
     bline2:=bufim[4*(x+SXb)+2,y+SYb];
     bline3:=bufim[4*(x+SXb)+3,y+SYb];
     bline0:=round(green*bline0);
     bline1:=round(blue*bline1);
     bline2:=round(green*bline2);
     bline3:=round(red*bline3);
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

procedure display3(f:boolean);
{Рисование картинки Image1 в 2 режиме}
var
x,y:word;
line : pByteArray;
bline:integer;
mash:integer;
begin
fillchar(gis,sizeof(gis),0);                       //обнуление массива гистограммы
mash:=round(0.0007*yccd*xccd);
if f then preobr;
 for y:=0 to yccd-1 do                                //1000 строк
  begin
  Line :=InImage.ScanLine[y];
   for x:=0 to xccd-1 do
    begin                                          //один пиксел в квартете
     bline:=bufim[x,y];
     bline:=bline shr iso;
     if bline > 255 then bline:=255;
     Line^[3*x]:=bline;                            //одинаковые цвета (Ч/Б изображение)
     Line^[3*x+1]:=bline;
     Line^[3*x+2]:=bline;
     inc(gis[bline]);
    end;
  end;
  Form1.Image1.Picture.Bitmap:=InImage;
  With Form1 do
  begin
   Image2.Canvas.FillRect(Image2.Canvas.ClipRect);
   Image2.Canvas.Pen.Color:=$c0c0c0;Image2.Canvas.MoveTo(0,Image2.Height-1);
   for x:=0 to 255 do Image2.Canvas.LineTo(x,Image2.Height-1-(gis[x] div mash));
  end;
end;

procedure display4;
{Рисование картинки Image3 в 2 режиме (лупа)}
var
x,y:word;
line,line1: pByteArray;
bline:integer;
begin
 for y:=0 to 49 do
  begin
  Line :=InImage3.ScanLine[2*y];
  Line1 :=InImage3.ScanLine[2*y+1];
    for x:=0 to 49 do
    begin
     bline:=bufim[x+SXb,y+SYb];

     bline:=bline shr iso;
     if bline > 255 then bline:=255;

     Line^[6*x]:=bline;
     Line^[6*x+1]:=bline;
     Line^[6*x+2]:=bline;

     Line^[6*x+3]:=bline;
     Line^[6*x+4]:=bline;
     Line^[6*x+5]:=bline;

     Line1^[6*x]:=bline;
     Line1^[6*x+1]:=bline;
     Line1^[6*x+2]:=bline;

     Line1^[6*x+3]:=bline;
     Line1^[6*x+4]:=bline;
     Line1^[6*x+5]:=bline;
    end;
  end;
  Form1.Image3.Picture.Bitmap:=InImage3;
end;

function Devices(Lines:TStrings): boolean;
{Опрос подключенных устройств и инициализация AD9822}
var
I : Integer;
begin
 result:=false;
 GetFTDeviceCount;
 Lines.Add('Всего устройств: '+IntToStr(FT_Device_Count));
 I := FT_Device_Count-1;
 while I >= 0 do
  begin
   Lines.Add('Устройство N'+IntToStr(I)+':');
   GetFTDeviceSerialNo(I);
   Lines.Add('Серийный номер: '+FT_Device_String);
   if pos('CAM8',FT_Device_String) <> 0 then result:=true;
   GetFTDeviceDescription(I);
   Lines.Add('Название: '+FT_Device_String);
   Lines.Add('');
   Dec(I);
  end;
  if result then
   begin
    if Open_USB_Device_By_Serial_Number(FT_CAM8A,'CAM8A') = FT_OK then
    Lines.Add('CAM8A открыт!');
    if Open_USB_Device_By_Serial_Number(FT_CAM8B,'CAM8B') = FT_OK then
    Lines.Add('CAM8B открыт!');
    if Set_USB_Device_BitMode(FT_CAM8B,$ff, $01) = FT_OK then             //установка BitMode
    Lines.Add('BitMode 1');

    Set_USB_Device_LatencyTimer(FT_CAM8B,2);       //максимальное быстродействие
    Set_USB_Device_LatencyTimer(FT_CAM8A,2);
    hev := TEvent.Create(nil, false, false, '');

    //Purge_USB_Device_In(FT_CAM8A);
    //Purge_USB_Device_In(FT_CAM8B);
    
    Purge_USB_Device(FT_CAM8A,FT_PURGE_RX);
    Purge_USB_Device(FT_CAM8B,FT_PURGE_RX);
    adress:=0;
    AD9822(0,$58);             //режим AD9822 - канал G,2 вольта опорность, CDS режим
    AD9822(1,$a0);             //смещение нулевое
    AD9822(6,14+256);
    AD9822(3,34);//$3f);       //усиление устанавливается такое. что не переполняется АЦП
    HC595($f9);//($2f);
    i:=Write_USB_Device_Buffer(FT_CAM8B,adress);

    Form1.Memo1.Lines.Add(inttostr(i));
   end;
end;

procedure Closedevices(Lines:TStrings);             //закрытие устройств
begin
 if Close_USB_Device(FT_CAM8A) = FT_OK then
 Lines.Add('CAM8A закрыт!');
 if Close_USB_Device(FT_CAM8B) = FT_OK then
 Lines.Add('CAM8B закрыт!');
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
 Memo1.Clear;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 inc(nnn);
 if nnn = 1 then                                        //через секунду после старта выключить питание +15V
  begin
   adress:=0;
   HC595($79);
   n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
  // Memo1.Lines.Add('power off');
  end;
 Form1.Caption:=inttostr(nnn);                          //отображать на заголовке секунды важержки
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
    red:=exp(0.05*TrackBar3.Position);
    green:=exp(0.05*TrackBar4.Position);
    blue:=exp(0.05*TrackBar5.Position);
   iso:=8;zap:=true;
end;

procedure TForm1.FormClose(Sender: TObject);                            //спасти настройки
begin
   Timer1.Enabled:=false;
   ConfigFile.WriteInteger('TIMES','T1',SpinEdit1.Value);
   ConfigFile.WriteInteger('COLORS','RED',TrackBar3.Position);
   ConfigFile.WriteInteger('COLORS','GREEN',TrackBar4.Position);
   ConfigFile.WriteInteger('COLORS','BLUE',TrackBar5.Position);
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
 //Memo1.Lines.Add(inttostr(SXb)+' '+inttostr(SYb));
  if CheckBox1.Checked then display4 else display2;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);                      //открытие - заккрытие устройств
begin
  if SpeedButton1.Down then
  begin
  if Devices(Memo1.Lines) then
   begin
     Button1.Enabled:=true;
     SpeedButton3.Enabled:=true;
     TrackBar1.Enabled:=true;
     Trackbar2.Enabled:=true;
   end else SpeedButton1.Down:=false;
  end else
   begin
    Timer1.Enabled:=false;
    Closedevices(Memo1.Lines);
    SpeedButton3.Enabled:=false;
    Button1.Enabled:=false;
    TrackBar1.Enabled:=false;
    Trackbar2.Enabled:=false;
   end;
end;

procedure TForm1.Button1Click(Sender: TObject);
{Кнопка "кадр"
Используются 2 таймера: timer2 - для реализации выдержки, которая устанавливается в SpeedEdit1 в миллисекундах,
                        timer1 - для выключения - включения питания 15 V ICX453
}
begin
adress:=0;                                                //перед накоплением надо "слить" накопленный заряд в матрице
dt0:=now;
shift2;
n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
zap:=true;

Timer2.Interval:=SpinEdit1.Value-19;
Timer2.Enabled:=true;
Timer1.Enabled:=true;nnn:=0;
end;

procedure TForm1.TrackBar1Change(Sender: TObject);               //изменение усиления AD9822
begin
 adress:=0;
 AD9822(3,TrackBar1.Position);
 Write_USB_Device_Buffer(FT_CAM8B,adress);
end;

procedure TForm1.TrackBar2Change(Sender: TObject);              //изменение смещения AD9822
var
x:word;
begin
 x:=abs(2*TrackBar2.Position);if TrackBar2.Position < 0 then x:=x+256;
 adress:=0;
 AD9822(6,x);
 Write_USB_Device_Buffer(FT_CAM8B,adress);
end;

procedure TForm1.RadioGroup3Click(Sender: TObject);             //изменение режима отображения
begin
 iso:= 8-RadioGroup3.ItemIndex;
 zap:=false;
 if CheckBox1.Checked then
  begin
   display3(false);
   display4;
  end else
  begin
   display(false);
   display2;
  end;
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);                    //хитрое изменение выдержки, мс
begin
SpinEdit1.Increment:=5;
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

procedure TForm1.Timer2Timer(Sender: TObject);
begin
Timer2.Enabled:=false;
Timer1.Enabled:=false;
   adress:=0;                                                //включить +15V
   HC595($f9);
   n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
 //  Memo1.Lines.Add('power on');

 if CheckBox1.Checked then                                   //собственно считать изображение и отобразить
 begin
  readframe2(true);
  hev.WaitFor(4000);
  display3(true);
  display4;
 end  else
 begin
  readframe(true);
  hev.WaitFor(4000);
  display(true);
  display2;
 end;

 n:=Get_USB_Device_QueueStatus(FT_CAM8A);      //проверка буфера на предмет байтов сколько
 Memo1.Lines.Add(inttostr(n));

 dt:=now-dt0;
 if CheckBox3.Checked then Button3.Click;
 if CheckBox2.Checked then Button1.Click;                    //зациклить, если опрос (CheckBox3.Checked)
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);         //сформировать массив bias. состоящий из нескольких bias (сделать master bias)
var
x,y:word;
max,i:byte;
begin
max:=SpinEdit2.Value;
 if SpeedButton3.Down then
  begin
   if CheckBox1.Checked then
    begin
     for i:=0 to max-1 do
      begin
       Memo1.Lines.Add(inttostr(i));
       readframe2(false);
       for y:= 0 to yccd-1 do
        for x:=0 to xccd-1 do bias[x,y]:=bias[x,y]+bufim[x,y];
      end;
       for y:= 0 to yccd-1 do
        for x:=0 to xccd-1 do bias[x,y]:=bias[x,y] div max;
    end  else
    begin
     for i:=0 to max-1 do
      begin
       Memo1.Lines.Add(inttostr(i));
       readframe(false);
       for y:= 0 to yccd-1 do
        for x:=0 to 4*xccd-1 do bias[x,y]:=bias[x,y]+bufim[x,y];
      end;
      for y:= 0 to yccd-1 do
        for x:=0 to 4*xccd-1 do bias[x,y]:=bias[x,y] div max;
    end;
    Memo1.Lines.Add('bias считан'); 
  end else
  begin
   for y:= 0 to yccd-1 do
    for x:=0 to 4*xccd-1 do bias[x,y]:=0;
  end;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
 red:=exp(0.05*TrackBar3.Position);
end;

procedure TForm1.TrackBar4Change(Sender: TObject);
begin
 green:=exp(0.05*TrackBar4.Position);
end;

procedure TForm1.TrackBar5Change(Sender: TObject);
begin
 blue:=exp(0.05*TrackBar5.Position);
end;

procedure TForm1.Button2Click(Sender: TObject);
var
x,y:integer;
median,skv,dsper:real;
begin
 median:=0;dsper:=0;
 for y:=0 to yccd-1 do for x:=0 to xccd-1 do median:=median+bufim[x,y];
   median:=median/(yccd*xccd);
   Memo1.Lines.Add('среднее = '+floattostrf(median,fffixed,6,2));
 for y:=0 to yccd-1 do for x:=0 to xccd-1 do dsper:=dsper+(bufim[x,y]-median)*(bufim[x,y]-median);
  dsper:=dsper/(xccd*yccd);
//    Memo1.Lines.Add(floattostrf(dsper,fffixed,6,2));
    skv:=sqrt(dsper);
    Memo1.Lines.Add('ср кв откл = '+floattostrf(skv,fffixed,6,2));
    Memo1.Lines.Add('время кадра = '+floattostrf(dt*86400,fffixed,6,2));
end;

procedure TForm1.Button3Click(Sender: TObject);
var
x,y:integer;
kol:integer;
line,line2:array[0..2999] of word;
l,l2:integer;
f:TFileStream;
begin
    kol:=kolbyte div (2*yccd);

    f:=TFileStream.Create(Form1.Edit1.Text+inttostr(nframe)+'.raw',fmCreate,fmShareExclusive);

     for y:=0 to yccd - 1 do
      begin
       l:=0;l2:=0;
       for x:=0 to kol - 1 do
        begin
         if x and 3 = 0 then begin line[l]:=bufim[x,y];inc(l) end;
         if x and 3 = 1 then begin line2[l2]:=bufim[x,y];inc(l2) end;
         if x and 3 = 2 then begin line2[l2]:=bufim[x,y];inc(l2) end;
         if x and 3 = 3 then begin line[l]:=bufim[x,y];inc(l) end;
        end;
       f.Write(line,kol);
       f.Write(line2,kol);
      end;
    f.Free;
    inc(nframe);
    Form1.Memo1.Lines.Add('кадр № '+inttostr(nframe-1)+' записан');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
 Button3.Enabled:=not(CheckBox1.Checked);
end;

end.

