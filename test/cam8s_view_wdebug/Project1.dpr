program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1};//,
//  icx453 in 'icx453.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
