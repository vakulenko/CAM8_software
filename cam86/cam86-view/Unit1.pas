unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, StdCtrls, MyD2XX, IniFiles, Cam86, ImCam, ExtCtrls,
  ComCtrls, Math, Spin;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Connect: TSpeedButton;
    Image1: TImage;
    Getimg: TButton;
    Image2: TImage;
    Gain: TTrackBar;
    Label1: TLabel;
    Offset: TTrackBar;
    Label2: TLabel;
    ComboBox1: TComboBox;
    Image3: TImage;
    Bin: TCheckBox;
    ROI: TCheckBox;
    Exposure: TLabel;
    RadioGroup1: TRadioGroup;
    Panel1: TPanel;
    information: TCheckBox;
    Timer1: TTimer;
    ProgressBar1: TProgressBar;
    Edit1: TEdit;
    WriteFits: TCheckBox;
    stop: TButton;
    Timer2: TTimer;
    Panel2: TPanel;
    ComboBox2: TComboBox;
    Continuously: TSpeedButton;
    SpinEdit1: TSpinEdit;
    Label4: TLabel;
    Cooling: TCheckBox;
    Label3: TLabel;
    delay: TSpinEdit;
    mode: TRadioGroup;
    Label5: TLabel;
    WriteFile: TButton;
    Button1: TButton;
    Timer3: TTimer;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    Button4: TButton;
    inf: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure ConnectClick(Sender: TObject);
    procedure GetimgClick(Sender: TObject);
    procedure FormMD(Sender: TObject; Button:                         //?????????? ??????? ??????????? ??? ??????????? ? "????"
TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GainChange(Sender: TObject);
    procedure OffsetChange(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure RisAll;
    procedure informationClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure stopClick(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure ContinuouslyClick(Sender: TObject);
    procedure ClearMemo(Sender: TObject);
    procedure CoolingClick(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
    procedure ShowInf;
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure modeClick(Sender: TObject);
    procedure WriteFitsClick(Sender: TObject);
    procedure WriteFileClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

const
razm = 50;  

var
  Form1: TForm1;
  ConfigFile:TIniFile;
  namest:string;
  ff:TMyImage;
  StartXb,StartYb,SXb,SYb,SY0:integer;
  iso:integer;
  nframe:integer;
  Offset0:integer;
  expo:single;
  dt0:TDateTime;
  ndelay:integer;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
 Show_mess:=nil;//Memo1.Lines;
 ff:=TMyImage.Create;
 ff.Width:=CameraWidth;
 ff.Height:=CameraHeight;
 ff.init;
 ff.bufim:=@bufim;
 ff.bufi2:=@bufi2;
 ff.pedestal:=500;
 SXb:= CameraWidth div 2;
 SYb:= CameraHeight div 2;
 ConfigFile:=TIniFile.Create(GetCurrentDir+'\cam86.ini');
 if not ConfigFile.SectionExists('NAME') then
  begin
   ConfigFile.WriteString('NAME','name','1.hex');
   namest:='1.hex';
  end else
  begin
   namest:= ConfigFile.ReadString('NAME','name','1.hex');
  end;
 if not ConfigFile.SectionExists('EXP') then
  begin
   ConfigFile.WriteInteger('EXP','exp',0);
   ComboBox1.ItemIndex:=0
  end else
  begin
   ComboBox1.ItemIndex:=ConfigFile.ReadInteger('EXP','exp',0)
  end;

  if not ConfigFile.SectionExists('COOL') then
  begin
   ConfigFile.WriteInteger('COOL','cool',4);
   ComboBox2.ItemIndex:=4;
  end else
  begin
   ComboBox2.ItemIndex:=ConfigFile.ReadInteger('COOL','cool',4)
  end;

  if not ConfigFile.SectionExists('OFFSET') then
  begin
   ConfigFile.WriteInteger('OFFSET','offset',0);
   Offset0:=0
  end else
  begin
   Offset0:=ConfigFile.ReadInteger('OFFSET','offset',0)
  end;

  iso:=-1;
  nframe:=0;
end;

procedure TForm1.FormClose(Sender: TObject);
begin
  ConfigFile.WriteInteger('EXP','exp',ComboBox1.ItemIndex);
  ConfigFile.WriteInteger('COOL','cool',ComboBox2.ItemIndex);
  ff.Free;
end;

procedure TForm1.RisAll;
begin
 if Bin.Checked then
  begin
   ff.ris(iso);
   ff.RisGist;
  end           else
  begin
   ff.Bayer;
   ff.risRGB(iso);
   ff.risGistRGB;
  end;
 Form1.Image1.Canvas.StretchDraw(Rect(0,0,Form1.Image1.Width,Form1.Image1.Height),ff.Image);
 Image2.Canvas.CopyRect(Rect(0,0,Form1.Image2.Width,Form1.Image2.Height),ff.Image.Canvas,Rect(SXb,SYb,SXb+razm,SYb+razm));
 Form1.Image3.Canvas.StretchDraw(Rect(0,0,Form1.Image3.Width,Form1.Image3.Height),ff.GisGraf);
end;

procedure TForm1.ConnectClick(Sender: TObject);
var
st:string;
begin
 st:='No camera';
 if Connect.Down then
  begin
   if CameraConnect then
                     begin
                      Gain.Enabled:=true;
                      Offset.Enabled:=true;
                      Offset.Position:=Offset0;
                      GetImg.Enabled:=true;
                      Stop.Enabled:=true;
                      Timer2.Enabled:=true;
                      st:='connect'
                     end
  end                 else
  begin
   if CameraDisConnect then
                        begin
                         sleep(600);
                         Gain.Enabled:=false;
                         Offset.Enabled:=false;
                         GetImg.Enabled:=false;
                         Stop.Enabled:=false;
                         Timer2.Enabled:=false;
                         st:='disconnect';
                         Cooling.Checked:=false;
                        end else st:='';
  end;
  Memo1.Lines.Add(st);
end;

procedure TForm1.GetimgClick(Sender: TObject);
var
st:string;
ex,errcode:integer;
mn:single;
y0,dy:integer;
begin
 if ROI.Checked then
  begin
   y0:=SYb-razm div 2;
   if y0 < 0 then y0:=0;
   dy:=2*razm;
   if y0 > CameraHeight-dy then y0:= CameraHeight-dy
  end else begin y0:=0;dy:=2000 end;
 //Memo1.Lines.Add(inttostr(y0)+' '+inttostr(dy)); 
 st:=ComboBox1.Items.Strings[ComboBox1.ItemIndex];
 if pos('ms',st) <> 0 then mn:=0.001;
 if pos('sec',st) <> 0 then mn:=1.0;
 if pos('min',st) <>0 then mn:=60.0;
 val(st,ex,errcode);
 expo:=mn*ex;
 ff.exposure:=expo;
 ProgressBar1.Max:=round(3*expo);
 ProgressBar1.Position:=0;
 Timer1.Enabled:=true;
 dt0:=Gettime;
 if Bin.Checked then CameraStartExposure (1,0,y0,3000,dy,expo,true)
                else CameraStartExposure (0,0,y0,3000,dy,expo,true);
                Application.ProcessMessages;
 repeat
  Application.ProcessMessages;
 until mImageReady=true;
 Timer1.Enabled:=false;
 ProgressBar1.Position:=0;
 if mode.ItemIndex = 1 then ff.DarkAdd;
 if mode.ItemIndex = 2 then ff.DarkSub;
 RisAll;

 dt0:=Gettime-dt0;

 if WriteFits.Checked then
  begin
   st:= Edit1.Text+inttostr(nframe)+'.fit';
   ff.FitFile(st);
   Memo1.Lines.Add('File '+st+' is recorded');
  end;
 if Information.Checked then Showinf;
 Button1.Click;
 if not inf.Checked then inc(nframe);
end;

procedure TForm1.FormMD(Sender: TObject; Button:            
TMouseButton; Shift: TShiftState; X, Y: Integer);
const
ykon = CameraHeight - razm;
xkon = CameraWidth - razm;
begin
  SXb := round(X*(CameraWidth/Image1.Width)-razm div 2);
  if SXb < 0 then SXb:=0;
  if SXb > xkon then SXb:=xkon;
  SYb := round(Y*(CameraHeight/Image1.Height)-razm div 2);
  if SYb < 0 then SYb:=0;
  if SYb > ykon then SYb:=ykon;
 //Memo1.Lines.Add(inttostr(SXb)+' '+inttostr(SYb));
 Image2.Canvas.CopyRect(Rect(0,0,Form1.Image2.Width,Form1.Image2.Height),ff.Image.Canvas,Rect(SXb,SYb,SXb+razm,SYb+razm));
end;

procedure TForm1.GainChange(Sender: TObject);
begin
 CameraSetGain(Gain.Position);
 Label1.Caption:=inttostr(Gain.Position);
end;

procedure TForm1.OffsetChange(Sender: TObject);
begin
 CameraSetOffset(Offset.Position);
 Label2.Caption:='Offset '+inttostr(Offset.Position);
 ConfigFile.WriteInteger('OFFSET','offset',Offset.Position);
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
 iso:=8-RadioGroup1.ItemIndex;
 RisAll;
end;

procedure TForm1.ShowInf;
begin
   Memo1.Lines.Add('Reading for '+floattostrf(24*3600*dt0,fffixed,5,2)+' sec');
   ff.Calk;
   Memo1.Lines.Add('StdDev (frame) = '+floattostrf(ff.skv,fffixed,6,2));
   Memo1.Lines.Add('StdDev (line) = '+floattostrf(ff.skv2,fffixed,6,2));
   Memo1.Lines.Add('MinValue = '+inttostr(ff.min)+' MaxValue = '+inttostr(ff.max));
   Memo1.Lines.Add('Mean = '+floattostrf(ff.meanc,fffixed,6,2));
   Memo1.Lines.Add('Meanr = '+floattostrf(ff.meanr,fffixed,6,2));
   Memo1.Lines.Add('Meang = '+floattostrf(ff.meang,fffixed,6,2));
   Memo1.Lines.Add('Meanb = '+floattostrf(ff.meanb,fffixed,6,2));
   Memo1.Lines.Add('')
end;

procedure TForm1.informationClick(Sender: TObject);
begin
 if information.Checked then Showinf;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
 ProgressBar1.Position:=ProgressBar1.Position+1;
end;

procedure TForm1.stopClick(Sender: TObject);
begin
 CameraStopExposure;
 Timer1.Enabled:=false;
 ProgressBar1.Position:=0;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
 if mCamerastate = 0 then
 begin
  if Continuously.Down then
   begin
    if ndelay = 0 then
     begin
      Memo1.Lines.Add('frame ¹ '+inttostr(nframe));
      GetImg.Click;
     end;
    if nframe >= SpinEdit1.Value then Continuously.Down:=false;
   end;
  end;
end;

procedure TForm1.ContinuouslyClick(Sender: TObject);
begin
 if Continuously.Down then
  begin
   nframe:=0;
   ndelay:=0;
   Timer3.Enabled:=true;
  end else Timer3.Enabled:=false;
end;

procedure TForm1.ClearMemo(Sender: TObject);
begin
 Memo1.Clear;
end;

procedure TForm1.CoolingClick(Sender: TObject);
var
 st:string;
 ex,errcode:integer;
 exr:single;
begin
 if Cooling.Checked then
 begin
  st:=ComboBox2.Items.Strings[ComboBox2.ItemIndex];
  val(st,ex,errcode);
  exr:=ex;
  CameraSetTemp(exr);
  CameraCoolingOn();
 end                else
 begin
  CameraCoolingOff();
 end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
var
 st:string;
 ex,errcode:integer;
 exr:double;
begin
 st:=ComboBox2.Items.Strings[ComboBox2.ItemIndex];
 val(st,ex,errcode);
 exr:=ex;
 CameraSetTemp(exr);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
 //ff.Dark0;
 ff.SaveDark(Edit1.Text);
 Memo1.Lines.Add('dark file recorded')
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
ff.ViewDark;
RisAll;
end;

procedure TForm1.modeClick(Sender: TObject);
begin
 if mode.ItemIndex = 1 then ff.Dark0;
end;

procedure TForm1.WriteFitsClick(Sender: TObject);
begin
 nframe:=0;
end;

procedure TForm1.WriteFileClick(Sender: TObject);
var
st:string;
begin
 st:= Edit1.Text+inttostr(nframe)+'.fit';
 ff.FitFile(st);
 Memo1.Lines.Add('File '+st+' is recorded');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 if mCamerastate <> 2 then
 begin
  Panel2.Caption:='T = '+floattostrf(CameraGetTemp(),fffixed,5,1)+' C';
 end;
end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
 inc(ndelay);
 if ndelay >= delay.Value then ndelay:=0; 
 //Memo1.Lines.Add(inttostr(ndelay));
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 if OpenDialog1.Execute then begin ff.LoadDark(OpenDialog1.FileName);
 Memo1.Lines.Add('dark file loaded') end;
end;

end.
