unit ImCam;

interface

Uses Graphics, SysUtils, Math;

type
 TMyImage = class(TObject)
  Width:integer;
  Height:integer;
  Image:TBitmap;
  GisGraf:TBitmap;
  bufim:PWordArray;
  bufi2:PWordArray;
  bufdark:array of single;
  ndark:integer;
  pedestal:integer;
  skv:single;
  skv2:single;
  meanc:single;
  meanr:single;
  meang:single;
  meanb:single;
  min:integer;
  max:integer;
  migis,magis,digis:integer;
  ccd_temp:single;
  exposure:single;
  gis:array[0..255] of integer;
  gisr:array[0..255] of integer;
  gisg:array[0..255] of integer;
  gisb:array[0..255] of integer;
  procedure Init;
  procedure Ris(iso:integer);
  procedure RisRGB(iso:integer);
  procedure RisGistRGB;
  procedure RisGist;
  procedure Bayer;
  procedure FitFile(filename:string);
  procedure Calk;
  procedure CalkGis;
  procedure Dark0;
  procedure DarkAdd;
  procedure DarkSub;
  procedure SaveDark(filename:string);
  procedure LoadDark(filename:string);
  procedure ViewDark;
end;

implementation

procedure TMyImage.Init;
begin
 Image:=TBitmap.Create;
 Image.PixelFormat:=pf24bit;
 Image.Height:=Height;
 Image.Width:=Width;
 GisGraf:=TBitmap.Create;
 GisGraf.Height:=128;
 GisGraf.Width:=256;
 GisGraf.PixelFormat:=pf24bit;
 GisGraf.Canvas.Brush.Color := clBlack;
 GisGraf.Canvas.FillRect(GisGraf.Canvas.ClipRect);
 ccd_temp:=0.0;
end;

procedure TMyImage.Ris(iso:integer);
var
x,y:integer;
line : pByteArray;
bline:integer;
begin
fillchar(gis,sizeof(gis),0);
if iso < 0 then CalkGis;
for y:=0 to Height-1 do
  begin
  Line :=Image.ScanLine[y];
  for x:=0 to Width-1 do
    begin
     bline:=bufim[x+y*Width];
     if iso < 0 then
      begin
       bline:=bline-migis;
       if bline < 0 then bline:=0;
       bline:=bline shl 8;
       bline:=bline div digis;
      end else
      begin
       bline:=bline shr iso;
      end;
     if bline > 255 then bline:=255;
     inc(gis[bline]);
     Line^[3*x+0]:=bline;
     Line^[3*x+1]:=bline;
     Line^[3*x+2]:=bline;
    end;
  end;
end;

procedure TMyImage.RisRGB(iso:integer);
var
x,y:integer;
line : pByteArray;
bliner,blineg,blineb:integer;
begin
fillchar(gisr,sizeof(gisr),0);
fillchar(gisg,sizeof(gisg),0);
fillchar(gisb,sizeof(gisb),0);
if iso < 0 then CalkGis;
for y:=0 to Height-1 do
  begin
  Line :=Image.ScanLine[y];
  for x:=0 to Width-1 do
    begin
     bliner:=bufi2[3*x+0+3*y*Width];
     blineg:=bufi2[3*x+1+3*y*Width];
     blineb:=bufi2[3*x+2+3*y*Width];
     if iso < 0 then
      begin
       bliner:=bliner-migis;
       if bliner < 0 then bliner:=0;
       bliner:=bliner shl 8;
       bliner:=bliner div digis;
       blineg:=blineg-migis;
       if blineg < 0 then blineg:=0;
       blineg:=blineg shl 8;
       blineg:=blineg div digis;
       blineb:=blineb-migis;
       if blineb < 0 then blineb:=0;
       blineb:=blineb shl 8;
       blineb:=blineb div digis;
      end else
      begin
       bliner:=bliner shr iso;
       blineg:=blineg shr iso;
       blineb:=blineb shr iso;
      end;

     if bliner > 255 then bliner:=255;
     if blineg > 255 then blineg:=255;
     if blineb > 255 then blineb:=255;
     inc(gisr[bliner]);
     inc(gisg[blineg]);
     inc(gisb[blineb]);
     Line^[3*x+0]:=blineb;
     Line^[3*x+1]:=blineg;
     Line^[3*x+2]:=bliner;
    end;
  end;
end;

procedure TMyImage.Calk;
var
x,y,a,b:integer;
tts:array of double;
begin
 SetLength(tts,Width*Height);
 for x:=0 to Width*Height-1 do tts[x]:=bufim[x];
 skv:=PopnStdDev(tts);
 meanc:=mean(tts);
 min:=round(MinValue(tts));
 max:=round(MaxValue(tts));
 SetLength(tts,2*Width);
 for x:=0 to 2*Width-1 do tts[x]:=bufim[x+500*Width];
 skv2:=PopnStdDev(tts);
 a:= Width div 2;
 b:=Height div 2;
 SetLength(tts,a*b);
 for y:=0 to b-1 do
  for x:=0 to a-1 do tts[x+y*a]:=bufim[2*x+1+(y*2+0)*Width];
 meanr:=mean(tts);
 for y:=0 to b-1 do
  for x:=0 to a-1 do tts[x+y*a]:=bufim[2*x+0+(y*2+1)*Width];
 meanb:=mean(tts);
 SetLength(tts,2*a*b);
 for y:=0 to b-1 do
  for x:=0 to a-1 do
   begin
    tts[x+(2*y+0)*a]:=bufim[2*x+0+(y*2+0)*Width];
    tts[x+(2*y+1)*a]:=bufim[2*x+1+(y*2+1)*Width];
   end;
 meang:=mean(tts); 
 tts:=nil;
end;

procedure TmyImage.RisGistRGB;
var
x,y:integer;
begin
//mash:=round(0.0005*Width*Height);
GisGraf.Canvas.FillRect(GisGraf.Canvas.ClipRect);
GisGraf.Canvas.Pen.Color:=$f00000;GisGraf.Canvas.MoveTo(0,GisGraf.Height-1);
for x:=0 to 255 do
 begin
  y:=round(8*ln(gisb[x]+1));
  GisGraf.Canvas.LineTo(x,GisGraf.Height-1-y);//(gisb[x] div mash));
 end;
GisGraf.Canvas.Pen.Color:=$00f000;GisGraf.Canvas.MoveTo(0,GisGraf.Height-1);
for x:=0 to 255 do
 begin
  y:=round(8*ln(gisg[x]+1));
  GisGraf.Canvas.LineTo(x,GisGraf.Height-1-y);//(gisg[x] div mash));
 end;
GisGraf.Canvas.Pen.Color:=$0000f0;GisGraf.Canvas.MoveTo(0,GisGraf.Height-1);
for x:=0 to 255 do
 begin
  y:=round(8*ln(gisr[x]+1));
  GisGraf.Canvas.LineTo(x,GisGraf.Height-1-y);//(gisr[x] div mash));
 end;
end;

procedure TmyImage.RisGist;
var
x,y:integer;
begin
//mash:=round(0.0005*Width*Height);
GisGraf.Canvas.FillRect(GisGraf.Canvas.ClipRect);
GisGraf.Canvas.Pen.Color:=$f0f0f0;GisGraf.Canvas.MoveTo(0,GisGraf.Height-1);
for x:=0 to 255 do
 begin
  y:=round(8*ln(gis[x]+1));
  GisGraf.Canvas.LineTo(x,GisGraf.Height-1-y);//(gisb[x] div mash));
 end;
end;

procedure TMyImage.Dark0;
var
x:integer;
begin
 SetLength(bufdark,Height*Width);
 for x:=0 to Height*Width-1 do bufdark[x]:=0;
 ndark:=0;
end;

procedure TMyImage.DarkAdd;
var
x:integer;
begin
 for x:=0 to Height*Width-1 do bufdark[x]:=bufdark[x]+bufim[x];
 inc(ndark);
end;

procedure TMyImage.DarkSub;
var
x:integer;
bbuf:integer;
begin
 for x:=0 to Height*Width-1 do
  begin
   bbuf:=bufim[x]-round(bufdark[x]/ndark)+pedestal;
   if bbuf < 0 then bufim[x]:=0 else bufim[x]:=bbuf;
  end;
end;

procedure TMyImage.SaveDark(filename:string);
var
f:file;
begin
assignfile(f,filename+'.drk');
rewrite(f,4);
blockwrite(f,ndark,1);
SetLength(bufdark,Height*Width);
blockwrite(f,bufdark[0],Height*Width);
closefile(f);
end;

procedure TMyImage.LoadDark(filename:string);
var
f:file;
begin
assignfile(f,filename);
reset(f,4);
blockread(f,ndark,1);
SetLength(bufdark,Height*Width);
blockread(f,bufdark[0],Height*Width);
closefile(f);
end;

procedure TMyImage.ViewDark;
var
bbuf:integer;
x:integer;
begin
for x:=0 to Height*Width-1 do
  begin
   bbuf:=round(bufdark[x]/ndark);
   if bbuf < 0 then bufim[x]:=0 else bufim[x]:=bbuf;
   if bbuf > 65535 then bufim[x]:=65535;
  end;
end;

procedure TMyImage.Bayer;
var
x,y:integer;
h,w:integer;
r11,g11,b11,r12,g12,b12,r21,g21,b21,r22,g22,b22:integer;
begin
 h:=Height div 2;
 w:=Width div 2;
 //11
 r11:=bufim[1];
 g11:=bufim[0];
 b11:=bufim[Width];
 bufi2[0]:=r11;
 bufi2[1]:=g11;
 bufi2[3]:=b11;
 //21
 r21:=bufim[Width-1];
 g21:=(bufim[Width-2]+bufim[2*Width-1]) div 2;
 b21:=bufim[2*Width-2];
 bufi2[3*(Width-1)+0]:=r21;
 bufi2[3*(Width-1)+1]:=g21;
 bufi2[3*(Width-1)+2]:=b21;
 //12
 r12:=bufim[Width*(Height-2)+1];
 g12:=(bufim[Width*(Height-2)+0]+bufim[Width*(Height-1)+1]) div 2;
 b12:=bufim[Width*(Height-1)];
 bufi2[3*Width*(Height-1)+0]:=r12;
 bufi2[3*Width*(Height-1)+1]:=g12;
 bufi2[3*Width*(Height-1)+2]:=b12;
 //22
 r22:=bufim[Width*(Height-1)-1];
 g22:=bufim[Width*Height-1];
 b22:=bufim[Width*Height-2];
 bufi2[3*Width*Height-3]:=r22;
 bufi2[3*Width*Height-2]:=g22;
 bufi2[3*Width*Height-1]:=b22;

 //перва€ строка
 for x:=0 to w-2 do
  begin
   r11:= bufim[(2*x+1)+(0)*Width];
   g11:=(bufim[(2*x+0)+(0)*Width]+bufim[(2*x+1)+(1)*Width]+bufim[(2*x+2)+(0)*Width]) div 3;
   b11:=(bufim[(2*x+0)+(1)*Width]+bufim[(2*x+2)+(1)*Width]) div 2;
   bufi2[3*(2*x+1)+0+(3*(0))*Width]:=r11;
   bufi2[3*(2*x+1)+1+(3*(0))*Width]:=g11;
   bufi2[3*(2*x+1)+2+(3*(0))*Width]:=b11;

   r21:=(bufim[(2*x+1)+(0)*Width]+bufim[(2*x+3)+(0)*Width]) div 2;
   g21:= bufim[(2*x+2)+(0)*Width];
   b21:= bufim[(2*x+2)+(1)*Width];
   bufi2[3*(2*x+2)+0+(3*(0))*Width]:=r21;
   bufi2[3*(2*x+2)+1+(3*(0))*Width]:=g21;
   bufi2[3*(2*x+2)+2+(3*(0))*Width]:=b21;
  end;
 y:=1999; //последн€€ строка
 for x:=0 to w-2 do
  begin
   r12:= bufim[(2*x+1)+(y-1)*Width];
   g12:= bufim[(2*x+1)+(y)*Width];
   b12:=(bufim[(2*x+0)+(y)*Width]+bufim[(2*x+2)+(y)*Width]) div 2;
   bufi2[3*(2*x+1)+0+(3*(y))*Width]:=r12;
   bufi2[3*(2*x+1)+1+(3*(y))*Width]:=g12;
   bufi2[3*(2*x+1)+2+(3*(y))*Width]:=b12;

   r22:=(bufim[(2*x+1)+(y-1)*Width]+bufim[(2*x+3)+(y-1)*Width]) div 2;
   g22:=(bufim[(2*x+1)+(y)*Width]+bufim[(2*x+2)+(y-1)*Width]+bufim[(2*x+3)+(y)*Width]) div 3;
   b22:= bufim[(2*x+2)+(y)*Width];
   bufi2[3*(2*x+2)+0+(3*(y))*Width]:=r22;
   bufi2[3*(2*x+2)+1+(3*(y))*Width]:=g22;
   bufi2[3*(2*x+2)+2+(3*(y))*Width]:=b22;
  end;
 x:=0;  //первый столбец
 for y:=0 to h-2 do
  begin
   r11:=(bufim[(x+1)+(2*y+0)*Width]+bufim[(x+1)+(2*y+2)*Width]) div 2;
   g11:=(bufim[(x+0)+(2*y+0)*Width]+bufim[(x+1)+(2*y+1)*Width]+bufim[(x+0)+(2*y+2)*Width]) div 3;
   b11:= bufim[(x+0)+(2*y+1)*Width];
   bufi2[3*(x)+0+(3*(2*y+1))*Width]:=r11;
   bufi2[3*(x)+1+(3*(2*y+1))*Width]:=g11;
   bufi2[3*(x)+2+(3*(2*y+1))*Width]:=b11;

   r12:=bufim[(x+1)+(2*y+2)*Width];
   g12:=bufim[(x+0)+(2*y+2)*Width];
   b12:=(bufim[(x+0)+(2*y+1)*Width]+bufim[(x+0)+(2*y+3)*Width]) div 2;
   bufi2[3*(x)+0+(3*(2*y+2))*Width]:=r12;
   bufi2[3*(x)+1+(3*(2*y+2))*Width]:=g12;
   bufi2[3*(x)+2+(3*(2*y+2))*Width]:=b12;
  end;
 x:=2999; //последний столбец
 for y:=0 to h-2 do
  begin
   r21:=(bufim[(x)+(2*y+0)*Width]+bufim[(x)+(2*y+2)*Width]) div 2;
   g21:= bufim[(x)+(2*y+1)*Width];
   b21:= bufim[(x-1)+(2*y+1)*Width];
   bufi2[3*(x)+0+(3*(2*y+1))*Width]:=r21;
   bufi2[3*(x)+1+(3*(2*y+1))*Width]:=g21;
   bufi2[3*(x)+2+(3*(2*y+1))*Width]:=b21;

   r22:=bufim[(x)+(2*y+2)*Width];
   g22:=(bufim[(x+0)+(2*y+1)*Width]+bufim[(x-1)+(2*y+2)*Width]+bufim[(x+0)+(2*y+3)*Width]) div 3;
   b22:=(bufim[(x-1)+(2*y+1)*Width]+bufim[(x-1)+(2*y+3)*Width]) div 2;
   bufi2[3*(x)+0+(3*(2*y+2))*Width]:=r22;
   bufi2[3*(x)+1+(3*(2*y+2))*Width]:=g22;
   bufi2[3*(x)+2+(3*(2*y+2))*Width]:=b22;
  end;
 // основной массив
  for y:=0 to h-2 do
   begin
    for x:=0 to w-2 do
     begin
      r11:=(bufim[(2*x+1)+(2*y+0)*Width]+bufim[(2*x+1)+(2*y+2)*Width]) div 2;
      g11:= bufim[(2*x+1)+(2*y+1)*Width];
      b11:=(bufim[(2*x+0)+(2*y+1)*Width]+bufim[(2*x+2)+(2*y+1)*Width]) div 2;
      bufi2[3*(2*x+1)+0+(3*(2*y+1))*Width]:=r11;
      bufi2[3*(2*x+1)+1+(3*(2*y+1))*Width]:=g11;
      bufi2[3*(2*x+1)+2+(3*(2*y+1))*Width]:=b11;

      r12:= bufim[(2*x+1)+(2*y+2)*Width];
      g12:=(bufim[(2*x+1)+(2*y+1)*Width]+bufim[(2*x+1)+(2*y+3)*Width]+bufim[(2*x+0)+(2*y+2)*Width]+bufim[(2*x+2)+(2*y+2)*Width]) div 4;
      b12:=(bufim[(2*x+0)+(2*y+1)*Width]+bufim[(2*x+0)+(2*y+3)*Width]+bufim[(2*x+2)+(2*y+3)*Width]+bufim[(2*x+2)+(2*y+1)*Width]) div 4;
      bufi2[3*(2*x+1)+0+(3*(2*y+2))*Width]:=r12;
      bufi2[3*(2*x+1)+1+(3*(2*y+2))*Width]:=g12;
      bufi2[3*(2*x+1)+2+(3*(2*y+2))*Width]:=b12;

      r22:=(bufim[(2*x+1)+(2*y+2)*Width]+bufim[(2*x+3)+(2*y+2)*Width]) div 2;
      g22:= bufim[(2*x+2)+(2*y+2)*Width];
      b22:=(bufim[(2*x+2)+(2*y+1)*Width]+bufim[(2*x+2)+(2*y+3)*Width]) div 2;
      bufi2[3*(2*x+2)+0+(3*(2*y+2))*Width]:=r22;
      bufi2[3*(2*x+2)+1+(3*(2*y+2))*Width]:=g22;
      bufi2[3*(2*x+2)+2+(3*(2*y+2))*Width]:=b22;

      r21:=(bufim[(2*x+1)+(2*y+0)*Width]+bufim[(2*x+3)+(2*y+0)*Width]+bufim[(2*x+1)+(2*y+2)*Width]+bufim[(2*x+3)+(2*y+2)*Width]) div 4;
      g21:=(bufim[(2*x+2)+(2*y+0)*Width]+bufim[(2*x+1)+(2*y+1)*Width]+bufim[(2*x+3)+(2*y+1)*Width]+bufim[(2*x+2)+(2*y+2)*Width]) div 4;
      b21:= bufim[(2*x+2)+(2*y+1)*Width];
      bufi2[3*(2*x+2)+0+(3*(2*y+1))*Width]:=r21;
      bufi2[3*(2*x+2)+1+(3*(2*y+1))*Width]:=g21;
      bufi2[3*(2*x+2)+2+(3*(2*y+1))*Width]:=b21;
     end;
   end;
end;

procedure TMyImage.CalkGis;
var
x,y,kol,kol2:integer;
gism:array [0..65535] of integer;
begin
 fillchar(gism,sizeof(gism),0);
 migis:=0;magis:=65535;
 kol:=0;kol2:=0;
 for y:=0 to Height-1 do
    for x:=0 to Width-1 do
      begin
       inc(gism[bufim^[x+y*Width]]);
      end;

     for x:=0 to 65535 do
      begin
       kol:=kol+gism[x];
       kol2:=kol2+gism[65535-x];
       if kol < Width then migis:=x;
       if kol2 < Width then magis:=65535-x;
      end;
      if migis = magis then magis:=migis+1;
      digis:=magis-migis;
end;

procedure TMyImage.FitFile(filename:string);
const
fitst0 ='SIMPLE  =                    T                                                  ';
fitst1 ='BITPIX  =                   16 /8 unsigned int, 16 & 32 int, -32 & -64 real     ';
fitst2 ='NAXIS   =                    2 /number of axes                                  ';
fitst3 ='NAXIS1  =                 1000 /fastest changing axis                           ';
fitst4 ='NAXIS2  =                 1000 /next to fastest changing axis                   ';
fitst5 ='BSCALE  =   1.0000000000000000 /physical = BZERO + BSCALE*array_value           ';
fitst6 ='BZERO   =   32768.000000000000 /physical = BZERO + BSCALE*array_value           ';
fitst7 ='INPUTFMT= ''Raw Binary'' /        Format of file from which image was read      ';
fitst8 ='CBLACK  =                    0 /Initial display black level in ADUs             ';
fitst9 ='CWHITE  =                 1024 /Initial display white level in ADUs             ';
fitst10='EXPOSURE=                  0.0 /exposure time in seconds                        ';
//fitst10='PEDESTAL=                    0 /Correction to add for zero-based ADU            ';
fitst11='CCD-TEMP=                 0.00 /CCD temperature in C                            ';
//fitst11='SWOWNER = ''ri      '' /          Licensed owner of software                      ';
fitst12='END';
var
x,y:integer;
f2:file;
probel:array [0..1916] of byte;
st:string;
st3:array[0..79] of char;
img:array[0..9999] of smallint;
begin
 CalkGis;

 fillchar(probel,sizeof(probel),$20);
 assignfile(f2,filename);
 rewrite(f2,1);
 blockwrite(f2,fitst0,80);
 blockwrite(f2,fitst1,80);
 blockwrite(f2,fitst2,80);
 st3:=fitst3;
 st:= inttostr(Width);
 while length(st) < 5 do st:=' '+st;
 st3[26]:=st[1];
 st3[27]:=st[2];
 st3[28]:=st[3];
 st3[29]:=st[4];
 st3[30]:=st[5];
 blockwrite(f2,st3,80);
 st3:=fitst4;
 st:= inttostr(Height);
 while length(st) < 5 do st:=' '+st;
 st3[26]:=st[1];
 st3[27]:=st[2];
 st3[28]:=st[3];
 st3[29]:=st[4];
 st3[30]:=st[5];
 blockwrite(f2,st3,80);
 blockwrite(f2,fitst5,80);
 blockwrite(f2,fitst6,80);
 blockwrite(f2,fitst7,80);
 st3:=fitst8;
 st:= inttostr(migis);
 while length(st) < 5 do st:=' '+st;
 st3[26]:=st[1];
 st3[27]:=st[2];
 st3[28]:=st[3];
 st3[29]:=st[4];
 st3[30]:=st[5];
       //st3:='    0';
 blockwrite(f2,st3,80);
 st3:=fitst9;
 st:= inttostr(magis);
 while length(st) < 5 do st:=' '+st;
 st3[26]:=st[1];
 st3[27]:=st[2];
 st3[28]:=st[3];
 st3[29]:=st[4];
 st3[30]:=st[5];
       //st3:=' 1000';
 blockwrite(f2,st3,80);
 //blockwrite(f2,fitst10,80);
 str(exposure:7:3,st);
 while length(st) < 7 do st:=' '+st;
 st3:=fitst10;
 st3[24]:=st[1];
 st3[25]:=st[2];
 st3[26]:=st[3];
 st3[27]:=st[4];
 st3[28]:=st[5];
 st3[29]:=st[6];
 st3[30]:=st[7];
 blockwrite(f2,st3,80);
 //blockwrite(f2,fitst11,80);
 str(ccd_temp:5:1,st);
 while length(st) < 5 do st:=' '+st;
 st3:=fitst11;
 st3[26]:=st[1];
 st3[27]:=st[2];
 st3[28]:=st[3];
 st3[29]:=st[4];
 st3[30]:=st[5];
 blockwrite(f2,st3,80);
 blockwrite(f2,fitst12,3);
 blockwrite(f2,probel,sizeof(probel));
  for y:=0 to Height-1 do
   begin
    for x:=0 to Width-1 do img[x]:=$80+swap(bufim^[x+y*Width]);
    blockwrite(f2,img,2*Width);
   end;
 closefile(f2);
end;

end.
