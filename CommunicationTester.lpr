program CommunicationTester;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
    {$ENDIF} {$IFDEF HASAMIGA}
  athreads,
    {$ENDIF}
    Interfaces, // this includes the LCL widgetset
    Forms, lazcontrols,
    mainunit,
    SerialManager, SerialPortFrameUnit { you can add units after this };

{$R *.res}

begin
    RequireDerivedFormResource := True;
  Application.Title:='CommunicationTester';
  Application.Scaled:=True;
    Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
    Application.Run;
end.
