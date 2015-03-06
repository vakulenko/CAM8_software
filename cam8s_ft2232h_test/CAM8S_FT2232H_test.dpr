program CAM8S_FT2232H_test;

uses
  Forms,
  ft2232h_test in '..\ft2232h_test\ft2232h_test.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
