// --------------------------------------------------------------------------------
// ASCOM Camera driver low-level interaction library for cam8s_v0.5
// Edit Log:
// Date		    	Who	 Vers	  Description
// -----------	---	-----	---------------------------------------------------------
// 24-sep-2014  VSS 0.5     Initial release (code obtained from grim)
// --------------------------------------------------------------------------------

library cam8sll05;

uses
  Classes,
  SysUtils,
  MyD2XX,
  Windows;

{$R *.res}
const CameraWidth  = 3000;    //ширина изображения
      CameraHeight = 2000;    //высота изображения
      portfirst = $e1+8; //$a1;        //первоначальное значение на выводах порта BDBUS
      portsecond = $91+8;//$c1;
      xccd = 1500;
      yccd = 1000;
      dx = 3044-2*xccd;
      dx2 = 1586-xccd;
      dy = 512-(yccd div 2);
      apolosa = 50;

type  camera_image_type = array [0..CameraHeight-1,0..CameraWidth-1] of integer;     //driver image type

      {Class for reading thread}
      posl = class(TThread)
      private
      { Private declarations }
      protected
      procedure Execute; override;
      end;
{GLobal variables}
var   IsConnected : boolean = false;        //переменная-флаг, отображает состояние соединения с камерой
      adress : integer;                     //указатель текущего адреса в выходном буфере FT2232HL
      mBin : integer;                       //биннинг,
      mImageReady : boolean = false;        //переменная-флаг, отображает готовность к считыванию кадра
      mCameraState : integer = 0;           //переменная-состояние камеры
      ExposureTimer, Timer15V : integer;    //таймер экспозиции и 15В таймер
      co: posl;                             //переменная для второго потока (чтение изображения)
      bufim :array[0..CameraWidth-1,0..CameraHeight-1] of word;       //буферный массив-изображение для операций
      mYn,mdeltY:integer;                   //начало чтения и количество по строкам
      mXn,mdeltX:integer;                   //начало чтения и количество по столбцам
      hTargetWnd: HWND;
      zatv:byte;

{ Небольшое пояснение работы с FT2232LH.
 Всегда используется такой прием:
  1. Вначале заполняется буфер и исходными байтами (необходимой последовательности импульсов на выводах порта BDBUS).
При этом инкрементируется указатель adress.
  2. Далее весь этот массив передается на выход командой: n:=Write_USB_Device_Buffer(FT_CAM8B,adress);
Замечательная микросхема FT2232HL честно без задержек все это передает на свой порт BDBUS. Передача 1 байта при этом занимает 65 нс.
Время отработки следующей команды n:=Write_USB_Device_Buffer(FT_CAM8B,adress) зависит от загруженности операционки и не контролируется
нами. Поэтому критическую последовательности импульсов нужно заполнять всю, а не передавать по очереди.
Благо програмный буфер драйвера это позволяет (в этой программе до 24 Мбайт!) Для этого нужно изменить текст D2XX.pas, я назвал его MyD2XX.pas}

procedure posl.Execute;                                     //собственно само чтение массива через порт ADBUS
{ Хитрое преобразование считанного буфера FT2232HL в буферный массив изображения
  из-за особенностей AD9822 считываем сначала старший байт, потом младший, а в delphi наоборот.
  Используем также  тип integer32, а не word16 из-за переполнения при последующих операциях }
var
x,y,x1:word;
begin
 for y:= mYn to mYn+mdeltY-1 do
  begin
   if mbin = 1 then
    begin
     Read_USB_Device_Buffer(FT_CAM8A,8*mdeltX);
     for x:=0 to mdeltX - 1 do
      begin
       x1:=x+mXn;
       bufim[2*x1,2*y]:=swap(FT_In_Buffer[4*x]);
       bufim[2*x1,2*y+1]:=swap(FT_In_Buffer[4*x+1]);
       bufim[2*x1+1,2*y+1]:=swap(FT_In_Buffer[4*x+2]);
       bufim[2*x1+1,2*y]:=swap(FT_In_Buffer[4*x+3]);
      end;
    end        else
    begin
     Read_USB_Device_Buffer(FT_CAM8A,2*mdeltX);
     for x:=0 to mdeltX - 1 do
      begin
       x1:=x+mXn;
       bufim[2*x1,2*y]:=swap(FT_In_Buffer[x]);
       bufim[2*x1+1,2*y]:=swap(FT_In_Buffer[x]);
       bufim[2*x1+1,2*y+1]:=swap(FT_In_Buffer[x]);
       bufim[2*x1,2*y+1]:=swap(FT_In_Buffer[x]);
      end;
    end;
  end;      
  mImageReady := true;
  hTargetWnd := FindWindowEx(0, 0, nil, PChar('CAM8'));
  if hTargetWnd <> 0 then  SendMessage(hTargetWnd, 2048, 0, DWORD(@bufim));//PChar('Ok!')));
end;

//создание потока с самоликвидацией
procedure ComRead;
begin
  co:=posl.Create(true);
  co.FreeOnTerminate:=true;
  co.Priority:=tpNormal;//Lower;//Normal;
  co.Resume;
end;

{Заполнение выходного буфера массивом для передачи и размещения байта val по адресу adr в микросхеме AD9822.
 Передача идет в последовательном коде.}
procedure AD9822(adr:byte;val:word);
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

 Write_USB_Device_Buffer(FT_CAM8B,@dan, kol);
end;

{procedure HC595(val:byte);
//Заполнение выходного буфера массивом для передачи байта val на выводы микросхемы HC595.
// Передача идет в последовательном коде.
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
end;}

procedure HC595(va:byte);
{Заполнение выходного буфера массивом для передачи байта val на выводы микросхемы HC595.
 Передача идет в последовательном коде.}
const
kol = 20;
var
dan:array[0..kol-1] of byte;
i:integer;
val:word;
begin
 fillchar(dan,kol,portfirst);                                    //заполняется массив первоначальным значением на выводах порта BDBUS
 if zatv = 1 then val:=va+$100 else val:=va;
 for i:=0 to 8 do
  begin
   dan[2*i+1]:=dan[2*i+1] + 2;
   if (val and $100) = $100 then begin dan[2*i]:=dan[2*i] + 4;dan[2*i+1]:=dan[2*i+1] + 4; end;
   val:=val*2;
  end;
   dan[18]:=dan[18]+ $80;
//   dan[19]:=dan[19]+ $80;

 for i:=0 to kol-1 do
  begin
   FT_Out_Buffer[2*i+adress]:=dan[i];
   FT_Out_Buffer[2*i+adress+1]:=dan[i];
  end;
 adress:=adress+2*kol;
end;

procedure shift0;
begin
// HC595($cb);
 HC595($db);
// HC595($da);
 HC595($fa);
// HC595($ea);
 HC595($ee);
// HC595($ce);
 HC595($cf);
end;

procedure shift;
{Заполнение выходного буфера массивом для одного шага вертикального сдвига}
begin
 HC595($cb);
//  HC595($cb);
 HC595($db);
//  HC595($db);
 HC595($da);
//  HC595($da);
 HC595($fa);
//  HC595($fa);
 HC595($ea);
//  HC595($ea);
 HC595($ee);
//  HC595($ee);
 HC595($ce);
//  HC595($ce);
 HC595($cf);
//  HC595($cf);
end;

procedure shift2;
{Заполнение выходного буфера массивом для "слива" накопленного изображения в сдвиговый регистр}
begin
 shift;

 HC595($c7);
 HC595($c7);
 HC595($c7);
 HC595($c7);

 HC595($cb);

 HC595($d9);
 HC595($d9);
 HC595($d9);
 HC595($d9);

 HC595($db);

 HC595($fa);//79);
 HC595($ea);//69);
 HC595($ee);//6d);
 HC595($ce);//2d);
 HC595($cf);//2f);
end;

procedure shift3;
{Заполнение выходного буфера массивом для одного шага вертикального сдвига + сигнал SUB для очитки области изображения}
begin
 HC595($cb);
 HC595($db);
 HC595($9a);//da);SUB
 HC595($ba);//fa);
 HC595($aa);//ea);
 HC595($ee);
 HC595($ce);
 HC595($cf);
// FT_Out_Buffer[adress]:=portsecond;
// inc(adress);
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
 for x:=0 to 6000+192-1+200 do
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

procedure clearline2;
const
dout : array[0..3] of byte = (portsecond,portsecond+8,portfirst+8,portfirst);
var x:integer;
begin

 for x:=0 to 79*xccd do
  begin
   FT_Out_Buffer[adress+0]:=dout[0];//dout[0]+$10;
   FT_Out_Buffer[adress+1]:=dout[0];//dout[1]+$10;
   FT_Out_Buffer[adress+2]:=dout[3];//dout[2]+$10;
   FT_Out_Buffer[adress+3]:=dout[3];//dout[3]+$10;
   inc(adress,4);
  end;

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


{Заполнение выходного буфера массивом и собственно сама операция чтения кадра в 1 режиме}
procedure readframe(bin:integer;expoz:integer);
const
dout : array[0..4] of byte = (portsecond,portsecond+8,portfirst+8,portfirst,portsecond+$28);
var x,y:integer;
begin
//camera reading ccd
mCameraState := 3;

Get_USB_Device_Status(FT_CAM8A);
if FT_Q_Bytes <> 0 then Purge_USB_Device_In(FT_CAM8A);
Get_USB_Device_Status(FT_CAM8B);
if FT_TxQ_Bytes <> 0 then Purge_USB_Device_Out(FT_CAM8B);

adress:=0;
if expoz > 52 then
 begin
  if expoz < 500 then
   begin
    shift3;
    for y:=0 to expoz-52 do
    for x:=0 to 416 do HC595($cf);
   end; 
  clearline2;
  clearframe;
 end         else
 begin
  clearline2;
  clearframe;
  shift3;
  if expoz > 0 then
  for y:=0 to expoz do
  for x:=0 to 416 do HC595($cf);
 end;
shift2;
Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);

adress:=0;
for y:=0 to dy-1+mYn do shift;
clearline;
Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
adress:=0;
comread;

for y:=0 to mdeltY -1 do
begin
adress:=0;
shift;

for x:=0 to dx-1+4*mXn do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;

if bin = 1 then
 begin

  for x:=0 to 4 do
   begin
    FT_Out_Buffer[adress+0]:=dout[0];
    FT_Out_Buffer[adress+1]:=dout[0];
    FT_Out_Buffer[adress+2]:=dout[3];
    FT_Out_Buffer[adress+3]:=dout[3];
    inc(adress,4);
   end;
   FT_Out_Buffer[adress+0]:=dout[0];
   FT_Out_Buffer[adress+1]:=dout[0];
   FT_Out_Buffer[adress+2]:=dout[3];
   FT_Out_Buffer[adress+3]:=dout[3]-8;
   inc(adress,4);

 for x:=0 to 4*mdeltX-2 do
  begin
   FT_Out_Buffer[adress+0]:=dout[0];
   FT_Out_Buffer[adress+1]:=dout[0];
   FT_Out_Buffer[adress+2]:=dout[0]-8;
   FT_Out_Buffer[adress+3]:=dout[3];
   FT_Out_Buffer[adress+4]:=dout[3];
   FT_Out_Buffer[adress+5]:=dout[3]-8;
   inc(adress,6);
   {FT_Out_Buffer[adress+0]:=dout[0];
   FT_Out_Buffer[adress+1]:=dout[0]-8;
   FT_Out_Buffer[adress+2]:=dout[3];
   FT_Out_Buffer[adress+3]:=dout[3]-8;
   inc(adress,4);  }
  end;
end        else
begin

 for x:=0 to 4 do
  begin
   FT_Out_Buffer[adress+0]:=dout[0];                               //?????????? ??? ??????, ???? ?????? ?????? RS ?????? ??? ??????? 4 - ?? ???????
   FT_Out_Buffer[adress+1]:=dout[0];
   FT_Out_Buffer[adress+2]:=dout[0]+$40;
   FT_Out_Buffer[adress+3]:=dout[0];
   FT_Out_Buffer[adress+4]:=dout[0]+$40;
   FT_Out_Buffer[adress+5]:=dout[0];
   FT_Out_Buffer[adress+6]:=dout[0]+$40;
   FT_Out_Buffer[adress+7]:=dout[0];
   FT_Out_Buffer[adress+8]:=dout[3];
   FT_Out_Buffer[adress+9]:=dout[3];
   inc(adress,10);
 end;
   FT_Out_Buffer[adress+0]:=dout[0];                               //?????????? ??? ??????, ???? ?????? ?????? RS ?????? ??? ??????? 4 - ?? ???????
   FT_Out_Buffer[adress+1]:=dout[0];
   FT_Out_Buffer[adress+2]:=dout[0]+$40;
   FT_Out_Buffer[adress+3]:=dout[0];
   FT_Out_Buffer[adress+4]:=dout[0]+$40;
   FT_Out_Buffer[adress+5]:=dout[0];
   FT_Out_Buffer[adress+6]:=dout[0]+$40;
   FT_Out_Buffer[adress+7]:=dout[0];
   FT_Out_Buffer[adress+8]:=dout[3];
   FT_Out_Buffer[adress+9]:=dout[3]-8;
   inc(adress,10);

 for x:=0 to mdeltX-2 do
  begin
   FT_Out_Buffer[adress+0]:=dout[0];                               //?????????? ??? ??????, ???? ?????? ?????? RS ?????? ??? ??????? 4 - ?? ???????
   FT_Out_Buffer[adress+1]:=dout[0]-8;
   FT_Out_Buffer[adress+2]:=dout[0]+$40;
   FT_Out_Buffer[adress+3]:=dout[0];
   FT_Out_Buffer[adress+4]:=dout[0]+$40;
   FT_Out_Buffer[adress+5]:=dout[0];
   FT_Out_Buffer[adress+6]:=dout[0]+$40;
   FT_Out_Buffer[adress+7]:=dout[0];
   FT_Out_Buffer[adress+8]:=dout[3];
   FT_Out_Buffer[adress+9]:=dout[3]-8;
   inc(adress,10);   
   {FT_Out_Buffer[adress+0]:=dout[0];                               //?????????? ??? ??????, ???? ?????? ?????? RS ?????? ??? ??????? 4 - ?? ???????
   FT_Out_Buffer[adress+1]:=dout[0];
   FT_Out_Buffer[adress+2]:=dout[0]-8;
   FT_Out_Buffer[adress+3]:=dout[0]+$40;
   FT_Out_Buffer[adress+4]:=dout[0];
   FT_Out_Buffer[adress+5]:=dout[0]+$40;
   FT_Out_Buffer[adress+6]:=dout[0];
   FT_Out_Buffer[adress+7]:=dout[0]+$40;
   FT_Out_Buffer[adress+8]:=dout[0];
   FT_Out_Buffer[adress+9]:=dout[3];
   FT_Out_Buffer[adress+10]:=dout[3];
   FT_Out_Buffer[adress+11]:=dout[3]-8;
   inc(adress,12);  }
  end;
end;

 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[0]-8;
 FT_Out_Buffer[adress+2]:=dout[3];
 FT_Out_Buffer[adress+3]:=dout[3];
 inc(adress,4);

for x:=0 to dx2-1+6000-4*mdeltX-4*mXn do
begin
 FT_Out_Buffer[adress+0]:=dout[0];
 FT_Out_Buffer[adress+1]:=dout[3];
 inc(adress,2);
end;
Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
end;    // comread;
 //Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);   //команда на вывод!!  }
end;

{Set camera gain, return bool result}
function CameraSetGain (val : integer) : WordBool; stdcall; export;
begin
 AD9822(3,val);           //усиление AD9822
 Result :=true;
end;

{Set camera offset, return bool result}
function CameraSetOffset (val : integer) : WordBool; stdcall; export;
var x : integer;
begin
 x:=abs(2*val);
 if val < 0 then x:=x+256;
 AD9822(6,x);                       //смещение AD9822
 Result :=true;
end;

{Connect camera, return bool result}
{Опрос подключенных устройств и инициализация AD9822}
function CameraConnect () : WordBool;  stdcall; export;
var  FT_flag, FT_OP_flag : boolean;
I : Integer;
begin
 FT_flag:=false;
 FT_Enable_Error_Report:=false;
 GetFTDeviceCount;
 I := FT_Device_Count-1;
 while I >= 0 do
  begin
   GetFTDeviceSerialNo(I);
   if pos('CAM8',FT_Device_String) <> 0 then FT_flag:=true;    //если обнаружен cam81 - подключаем
   GetFTDeviceDescription(I);
   Dec(I);
  end;
  FT_OP_flag:=true;
  if FT_flag then
   begin
   // FT_OP_flag:=true;
    if Open_USB_Device_By_Serial_Number(FT_CAM8A,'CAM8A') <> FT_OK then FT_OP_flag := false;
    if Open_USB_Device_By_Serial_Number(FT_CAM8B,'CAM8B')  <> FT_OK then FT_OP_flag := false;
    if Set_USB_Device_BitMode(FT_CAM8B,$ff, $01)  <> FT_OK then FT_OP_flag := false;             // BitMode

    Set_USB_Device_LatencyTimer(FT_CAM8B,2);       //максимальное быстродействие
    Set_USB_Device_LatencyTimer(FT_CAM8A,2);
    Set_USB_Device_TimeOuts(FT_CAM8A,4000,4000);
    Set_USB_Device_TimeOuts(FT_CAM8B,4000,4000);

    Purge_USB_Device_In(FT_CAM8A);
    Purge_USB_Device_OUT(FT_CAM8A);
    Purge_USB_Device_OUT(FT_CAM8B);

    AD9822(0,$58);             //режим AD9822 - канал G,2 вольта опорность, CDS режим
    AD9822(1,$a0);
    //AD9822(6,14+256);          //смещение -7
    AD9822(6,0);
    AD9822(3,34);//$3f);       //усиление устанавливается такое. что не переполняется АЦП
    adress:=0;
    HC595($cf);//($2f);
    Write_USB_Device_Buffer(FT_CAM8B,@FT_Out_Buffer,adress);
   end;
 IsConnected := FT_flag and FT_OP_flag;
 Result := FT_flag and FT_OP_flag;
end;

{Disconnect camera, return bool result}
function CameraDisConnect (): WordBool; stdcall; export;
var FT_OP_flag : boolean;
begin
 FT_OP_flag := true;
 if Close_USB_Device(FT_CAM8A) <> FT_OK then FT_OP_flag := false;   //закрытие устройств
 if Close_USB_Device(FT_CAM8B) <> FT_OK then FT_OP_flag := false;
 IsConnected := not FT_OP_flag;
 Result:= FT_OP_flag;
end;

{Get camera state, return int result}
function CameraGetCameraState : integer; stdcall; export;
begin
 Result := mCameraState;
end;

procedure ExposureTimerTick;  stdcall;
begin
 KillTimer (0,ExposureTimer);
 mCameraState := 3;

 adress:=0;
 HC595($cf);                //on +15V
 Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);

 readframe (mBin, 1000);

 //mImageReady := true;
end;

//off +15V
procedure Timer15VTick; stdcall;
begin
 KillTimer (0,Timer15V);
 adress:=0;
 HC595($4f);
 Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);
end;

{Check camera connection, return bool result}
function CameraIsConnected () : WordBool; stdcall; export;
begin
  Result := IsConnected;
end;

function CameraStartExposure (Bin,StartX,StartY,NumX,NumY : integer; Duration : double; light : WordBool) : WordBool; stdcall; export;
begin
 mBin := Bin;             //???????
 if light then zatv:=0 else
  zatv:=1;
 if (NumY+StartY > CameraHeight)or(StartY < 0)or(NumY <= 0) then begin mYn:=0;mdeltY:=yccd end
 else begin mYn:=StartY div 2;mdeltY:=NumY div 2 end;
 if (NumX+StartX > CameraWidth)or(StartX < 0)or(NumX <= 0) then begin mXn:=0;mdeltX:=xccd end
 else begin mXn:=StartX div 2;mdeltX:=NumX div 2 end;
 mImageReady := false;
 //camera exposing
 mCameraState := 2;
 if Duration > 0.499 then
 begin
 adress:=0;      // clearwin
 shift3;
 Write_USB_Device_Buffer(FT_CAM8B,@FT_OUT_Buffer,adress);
 ExposureTimer := settimer (0,0,round(Duration*1000-52),@ExposureTimerTick);
 Timer15V := settimer (0,0,1000,@Timer15VTick);
 end                   else
 begin
  mCameraState := 3;
  readframe (mBin,round(Duration*1000));
  //mImageReady := true;
 end;

 Result := true;

end;

{Check ImageReady flag, is image ready for transfer - transfer image to driver and return bool ImageReady flag}
function CameraGetImageReady : WordBool; stdcall; export;
begin
 Result := mImageReady;
end;

{Get back pointer to image}
function CameraGetImage : dword; stdcall; export;
begin
 //camera download ccd
 mCameraState := 4;
 Result := dword(@bufim);
 //camera idle
 mCameraState := 0;
end;

//????? ?????????? ???????????, ???????????? ???????, ????????? ????????? ?????? ?????

exports CameraConnect;
exports CameraIsConnected;
exports CameraDisConnect;
exports CameraSetGain;
exports CameraSetOffset;
exports CameraStartExposure;
exports CameraGetCameraState;
exports CameraGetImageReady;
exports CameraGetImage;

begin
end.

