unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Math, QFileCtrls, MyD2XX, IniFiles;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    Button2: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    OpenDialog1: TOpenDialog;
    Button16: TButton;
    Button11: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure MeClear;
    procedure Memo1Change(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button12Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
    procedure Button14Click(Sender: TObject);
    procedure Button15Click(Sender: TObject);
    procedure Button16Click(Sender: TObject);
    procedure Button11Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
disdev = 'CAM86 B';
portfirst = $0;

var
  Form1: TForm1;
  FT_op:boolean;
  siin:  array[0..63] of byte;
  siout: array[0..63] of byte;
  eep:   array[0..1023] of byte;
  prog,progw:  array[0..32768] of byte;
  maxprog,maxeep:word;
  pagesize:byte;
  maxc:integer;
  ft:text;
  namest:string;
  ConfigFile:TIniFile;
  pr:boolean;

implementation

{$R *.dfm}

function IntToBin(Value: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to 7 do
    if Value and (1 shl i) > 0 then
      result := '1' + result
    else
      result := '0' + result;
end;

function BinToInt(str :string): integer;
var
n:integer;
st:string;
begin
result:=0;
st:=str;
for n:=1 to 8 do if st[8-n+1] = '1' then result:=result+round(power(2,n-1));
end;

procedure sspi(kol:byte);
var
i,j:integer;
b:byte;
n:word;
begin
n:=16*kol+16;
FillChar(FT_Out_Buffer,n,0);
for j:=0 to kol-1 do
begin
b:=siin[j];
For i:= 0 to 7 do
begin
 inc(FT_Out_Buffer[2*i+1+16*j],$20);         //B5
 if (b and $80) = $80 then begin inc(FT_Out_Buffer[2*i+16*j],$80);inc(FT_Out_Buffer[2*i+1+16*j],$80);end;    //B3
 b:=b*2;
end;
end;
Write_USB_Device_Buffer(FT_HANDLEB,@FT_Out_Buffer,n);
end;

procedure sspo(kol:byte);
var i,j:integer;
b:byte;
n:word;
begin
 n:=16*kol+16;
 Read_USB_Device_Buffer(FT_HANDLEB,n);
 for j:=0 to kol-1 do
    begin
     b:=0;
     for i:=0 to 7 do
      begin
       b:=b*2;
       if (FT_In_Buffer[2*i+j*16+2] and $40) <> 0 then inc(b);        //B4
      end;
      siout[j]:=b;
    end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
sign:array[0..1] of word;
st:string;
begin
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);

siin[0]:=$ac;
siin[1]:=$53;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
siin[0]:=$ac;
siin[1]:=$53;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
Memo1.Lines.Add(inttohex(siout[0],2)+' '+inttohex(siout[1],2)+' '+inttohex(siout[2],2)+' '+inttohex(siout[3],2));

siin[0]:=$30;
siin[1]:=$0;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
sign[0]:=siout[3];

siin[0]:=$30;
siin[1]:=$0;
siin[2]:=$1;
siin[3]:=$0;
sspi(4);
sspo(4);
sign[1]:=siout[3];

siin[0]:=$30;
siin[1]:=$0;
siin[2]:=$2;
siin[3]:=$0;
sspi(4);
sspo(4);
sign[1]:=sign[1]+256*siout[3];

Memo1.Lines.Add('');
Memo1.Lines.Add('signature: '+inttohex(sign[0],2)+inttohex(sign[1],4));

if sign[0] =$1E then
 begin
  case sign[1] of
   $0f92:begin st:='ATmega48';pagesize:=32;maxeep:=256;maxprog:=4096 end;
   $0a92:begin st:='ATmega48P';pagesize:=32;maxeep:=256;maxprog:=4096 end;
   $0a93:begin st:='ATmega88';pagesize:=32;maxeep:=512;maxprog:=8192 end;
   $0f93:begin st:='ATmega88P';pagesize:=32;maxeep:=512;maxprog:=8192 end;
   $0694:begin st:='ATmega168';pagesize:=64;maxeep:=512;maxprog:=16384 end;
   $0b94:begin st:='ATmega168P';pagesize:=64;maxeep:=512;maxprog:=16384 end;
   $1495:begin st:='ATmega328';pagesize:=64;maxeep:=1024;maxprog:=32768 end;
   $0f95:begin st:='ATmega328P';pagesize:=64;maxeep:=1024;maxprog:=32768 end;
   end;
 end else st:='unknown signature';
 maxc:=maxprog;
 Memo1.Lines.Add(st);
 
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 Show_mess:=Memo1.Lines;
 ConfigFile:=TIniFile.Create(GetCurrentDir+'\prog.ini');
 if not ConfigFile.SectionExists('NAME') then
  begin
   ConfigFile.WriteString('NAME','name','1.hex');
   namest:='1.hex';
  end else
  begin
   namest:= ConfigFile.ReadString('NAME','name','1.hex');
  end;
  pr:=false;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
i:integer;
begin
 if Speedbutton1.Down then
  begin
   GetFTDeviceCount;
   for i:=0 to FT_Device_Count-1 do
   begin
    Memo1.Lines.Add('');
    Memo1.Lines.Add('device №'+inttostr(i)+' :');
    GetFTDeviceSerialNo(i);
    GetFTDeviceDescription(i);
    if pos(disdev,FT_Device_String) <> 0 then
     begin
      Open_USB_Device(i,FT_HANDLEB);
      Set_USB_Device_BitMode(FT_HANDLEB,$bf, $4);        //B4
      FT_Current_Baud:=60000;
      Set_USB_Device_BaudRate(FT_HANDLEB);
      Set_USB_Device_LatencyTimer(FT_HANDLEB,2);       //максимальное быстродействие
      Set_USB_Device_TimeOuts(FT_HANDLEB,400,400);

      Purge_USB_Device_In(FT_HANDLEB);
      Purge_USB_Device_Out(FT_HANDLEB);
      FT_op:=true;
      Break;
     end;
   end;
  end                 else
  begin
   Memo1.Lines.Add('');
   if FT_op then Close_USB_Device(FT_HANDLEB);
  end;
end;

procedure TForm1.MeClear;
begin
 Form1.Memo1.Clear;
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
 Memo1.Clear;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
siin[0]:=$ac;
siin[1]:=$80;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
Memo1.Lines.Add(inttohex(siout[0],2)+' '+inttohex(siout[1],2)+' '+inttohex(siout[2],2)+' '+inttohex(siout[3],2));
end;

procedure TForm1.Button4Click(Sender: TObject);
var
b:byte;
begin
 siin[0]:=$ac;
 siin[1]:=$a0;
 siin[2]:=$0;
 b:= BinToInt(Edit2.Text);// or $81;
 //b:= b and $df;
 siin[3]:=b;
 sspi(4);
 sspo(4);
 Memo1.Lines.Add(inttohex(siin[3],2));

 siin[0]:=$ac;
 siin[1]:=$a4;
 siin[2]:=$0;
 //b:= BinToInt(Edit2.Text);// or $81;
 //b:= b and $df;
 siin[3]:=$ff;
 sspi(4);
 sspo(4);
 Memo1.Lines.Add(inttohex(siin[3],2));

end;

procedure TForm1.Button12Click(Sender: TObject);
var
i,j:word;
st:string;
f:file;
k:integer;
begin
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);
for i:=0 to maxeep-1 do
 begin
  siin[0]:=$a0;
  siin[1]:=hi(i);
  siin[2]:=lo(i);
  siin[3]:=$0;
  sspi(4);
  sspo(4);
  eep[i]:=siout[3];
  j:=i and $000f;
  if j = 0 then st:=inttohex(i div 16,4)+':';
  st:=st+' '+inttohex(siout[3],2);
  if j = 15 then Memo1.Lines.Add(st);
 end;
assignfile(f,'eeprom.bin');
rewrite(f,maxeep);
blockwrite(f,eep,1);
closefile(f);
end;

procedure TForm1.Button13Click(Sender: TObject);
var
i,k:word;
f:file;
begin
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);
assignfile(f,'eeprom.bin');
 {$I-}
reset(f,maxeep);
 {$I+}
 if ioresult = 0 then
 begin
  blockread(f,eep,1);
  closefile(f);
  for i:=0 to maxeep-1 do
   begin
    siin[0]:=$c0;
    siin[1]:=hi(i);
    siin[2]:=lo(i);
    siin[3]:=eep[i];
    sspi(4);
    //sspo(4);
    sleep(5);
    k:=i and $000f;
    if k = 0 then Memo1.Lines.Add(inttohex(i,4));
   end;
  Memo1.Lines.Add('eeprom write'); 
 end else Memo1.Lines.Add('errors eeprom file');
end;

procedure TForm1.Button14Click(Sender: TObject);
var i,j,k,l:integer;
st:string;
f:file;
co:integer;
begin
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);
k:=0;
l:=maxprog div 16;
for j:=0 to l-1 do
 begin
  co:=0;
  st:=inttohex(j*16,4)+':';
  for i:=0 to 7 do
   begin
    siin[0+8*i]:=$20;
    siin[1+8*i]:=hi(k);
    siin[2+8*i]:=lo(k);
    siin[3+8*i]:=$0;
    siin[4+8*i]:=$28;
    siin[5+8*i]:=hi(k);
    siin[6+8*i]:=lo(k);
    siin[7+8*i]:=$0;
    inc(k,1);
   end;
    sspi(64);
    sspo(64);
  for i:=0 to 7 do
   begin
    prog[2*i+j*16]:=siout[3+8*i];
    prog[2*i+1+j*16]:=siout[7+8*i];
    if pr then
     begin
      co:=co+progw[2*i+j*16]-siout[3+8*i];
      co:=co+progw[2*i+1+j*16]-siout[7+8*i];
     end;
    st:=st+' '+inttohex(siout[3+8*i],2)+' '+inttohex(siout[7+8*i],2);
   end;
  Memo1.Lines.Add(st);
  if co <> 0 then begin Memo1.Lines.Add('error recorded'); end;
  if maxc < (j+1)*16 then exit;
 end;
assignfile(f,'prog.bin');
rewrite(f,maxprog);
blockwrite(f,prog,1);
closefile(f);
end;

procedure TForm1.Button15Click(Sender: TObject);
var
st,st1:string;
filename:string;
f:file;
nn,adress:integer;
i:integer;
iadress:word;
j:integer;
k:integer;
procedure wrme;
var
i,j,k,l:integer;
begin
 l:=maxprog div pagesize;
 l:=l div 2;
 for j:=0 to l-1 do
 begin
  for i:=0 to pagesize-1 do
    begin
     siin[0]:=$40;
     siin[1]:=$0;
     siin[2]:=i;
     siin[3]:=prog[2*i+j*2*pagesize];
     siin[4]:=$48;
     siin[5]:=$0;
     siin[6]:=i;
     siin[7]:=prog[2*i+1+j*2*pagesize];
     sspi(8);
    end;
  Read_USB_Device_Buffer(FT_HANDLEB,(16*8+16)*pagesize);
  k:=pagesize*j;
  siin[0]:=$4c;
  siin[1]:=hi(k);
  siin[2]:=lo(k);
  siin[3]:=$0;
  sspi(4);
  sleep(5);
  Purge_USB_Device_In(FT_HANDLEB);
  Purge_USB_Device_Out(FT_HANDLEB);
  Memo1.Lines.Add('Page № '+ inttostr(j));
  if maxc < (j+1)*2*pagesize then exit;
 end;
end;

begin
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);
FillChar(prog,sizeof(prog),$ff);maxc:=0;
OpenDialog1.InitialDir:= ExtractFilePath(namest);
OpenDialog1.Filter:='Hex files only|*.hex';
Memo1.Lines.Add(OpenDialog1.InitialDir);
if OpenDialog1.Execute then
 begin
  filename:=OpenDialog1.FileName;
  ConfigFile.WriteString('NAME','name',filename);
  // begin
    assignfile(ft,filename);
    reset(ft);
    repeat
     readln(ft,st);
     if st[1] <> ':' then
     begin
      Memo1.Lines.Add('Ошибка файла! - возможно не hex-файл');
      beep;
      exit;
     end;
     st1:='$'+copy(st,2,2);
     trystrtoint(st1,nn);     //кол-во байт
     st1:='$'+copy(st,4,4);
     trystrtoint(st1,adress); //адрес
     i:=nn;iadress:=adress;
     j:=10;
     while i>0 do
      begin
       st1:='$'+copy(st,j,2);
       trystrtoint(st1,k);
       if iadress < maxprog then prog[iadress]:=lo(k);
       if iadress > maxc then maxc:=iadress;
       dec(i);inc(j,2);
       inc(iadress);
      end;
    until eof(ft);
    closefile(ft);

    wrme;
    for i:=0 to maxprog-1 do progw[i]:=prog[i];
    pr:=true;
    Memo1.Lines.Add('готово');
   //end;
  end;

end;

procedure TForm1.Button16Click(Sender: TObject);
begin
FillChar(FT_Out_Buffer,1000,portfirst+$10);
Purge_USB_Device_In(FT_HANDLEB);
Purge_USB_Device_Out(FT_HANDLEB);
Write_USB_Device_Buffer(FT_HANDLEB,@FT_Out_Buffer,200);
end;

procedure TForm1.Button11Click(Sender: TObject);
begin
siin[0]:=$58;
siin[1]:=$08;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
Edit1.Text:=IntToBin(siout[3]);
siin[0]:=$50;
siin[1]:=$0;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
Edit2.Text:=IntToBin(siout[3]);
siin[0]:=$50;
siin[1]:=$08;
siin[2]:=$0;
siin[3]:=$0;
sspi(4);
sspo(4);
Edit3.Text:=IntToBin(siout[3]);
end;

end.
