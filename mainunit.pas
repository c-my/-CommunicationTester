unit mainunit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    ComCtrls, MaskEdit, SpinEx, Synaser, SerialManager,
    SerialPortFrameUnit, TCPFrameUnit;

type

    { TMainForm }

    TMainForm = class(TForm)
        MainPageControl: TPageControl;
        SerialPortFrame1: SerialPortFrame;
        SerialTabSheet: TTabSheet;
        TabSheet2: TTabSheet;

        //procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);


    private
        serialThr: SerialThread;
        serialPort: TBlockSerial;

    public

    end;

var
    MainForm: TMainForm;


implementation

{$R *.lfm}

{ TMainForm }


//procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
//begin
//    CloseSerial();
//    FreeAndNil(serialPort);
//end;




end.
