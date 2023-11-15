unit SerialManager;

{$mode ObjFPC}{$H+}
{$m+}
interface

uses
    Classes, Synaser;

type
    TRecvDataEvent = procedure(Data: string) of object;

    { SerialThread }

    SerialThread = class(TThread)
    protected
        procedure Execute; override;
        procedure ProcessRecvEvent;
    private
        MserialPort: TBlockSerial;
        FOnRecvData: TRecvDataEvent;
        currentRecvData: string;

    public
        constructor Create(CreateSuspended: boolean; serialPort: TBlockSerial);
        property OnRecvData: TRecvDataEvent read FOnRecvData write FOnRecvData;

    end;

implementation

constructor SerialThread.Create(CreateSuspended: boolean; serialPort: TBlockSerial);
begin
    inherited Create(CreateSuspended);
    FreeOnTerminate := True;
    Self.MserialPort := serialPort;

end;

procedure SerialThread.Execute;

begin
    while not (Terminated) do
    begin
        currentRecvData := MserialPort.RecvPacket(-1);
        Synchronize(@ProcessRecvEvent);
    end;
end;

procedure SerialThread.ProcessRecvEvent;
begin
    if Assigned(FOnRecvData) then
    begin
        FOnRecvData(currentRecvData);
    end;
end;

end.
