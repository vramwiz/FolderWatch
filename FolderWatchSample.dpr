program FolderWatchSample;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {FormMain},
  FolderWatch in 'FolderWatch.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
