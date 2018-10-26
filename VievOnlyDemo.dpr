program VievOnlyDemo;

uses
  Vcl.Forms,
  MainU in 'MainU.pas' {FormMain},
  ViewOnlyU in 'ViewOnlyU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
