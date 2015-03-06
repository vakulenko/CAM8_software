program cam8s_ft2232h_test_prj;

uses
  Forms,
  cam8s_ft2232h_test in 'cam8s_ft2232h_test.pas' {main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tmain, main);
  Application.Run;
end.
