unit SerialManager;

{$mode ObjFPC}{$H+}
{$m+}
interface

uses
    Classes, SysUtils, Synaser;

type
    TRecvDataEvent = procedure(Data: string) of object;
    TConnectedEvent = procedure of object;
    TConnectedFailedEvent = procedure of object;
    TClosedEvent = procedure of object;

    { SerialThread }

    SerialThread = class(TThread)
    protected
        procedure Execute; override;
        procedure ProcessRecvEvent;
        procedure ProcessConnectedEvent;
        procedure ProcessConnectedFailedEvent;
        procedure ProcessOnCloseEvent;
    private
        MserialPort: TBlockSerial;
        MSerialName: string;
        FOnRecvData: TRecvDataEvent;
        FOnConnected: TConnectedEvent;
        FOnClosed: TClosedEvent;
        FOnConnectedFailed: TConnectedFailedEvent;
        currentRecvData: string;

    public
        constructor Create(CreateSuspended: boolean; serialName: string);
        destructor Destroy; override;

        procedure Close();
        procedure SendString(Data: ansistring);
        function SendBuffer(buffer: pointer; length: integer): integer;
        procedure ConfigureSerial(baud, bits: integer; parity: char;
            stop: integer; softflow, hardflow: boolean);

        property OnRecvData: TRecvDataEvent read FOnRecvData write FOnRecvData;
        property OnConnected: TConnectedEvent read FOnConnected write FOnConnected;
        property OnConnectedFailed: TConnectedFailedEvent
            read FOnConnectedFailed write FOnConnectedFailed;
        property OnClosed: TClosedEvent read FOnClosed write FOnClosed;

        function isConnected(): boolean;
    end;

implementation

constructor SerialThread.Create(CreateSuspended: boolean; serialName: string);
begin
    inherited Create(CreateSuspended);
    FreeOnTerminate := True;
    self.MSerialName := serialName;
    Self.MserialPort := TBlockSerial.Create;

end;

destructor SerialThread.Destroy;
begin
    inherited Destroy;
    FreeAndNil(MserialPort);
end;

procedure SerialThread.Close;
begin
    MserialPort.Purge();
    MserialPort.CloseSocket();
    Terminate();
    FreeAndNil(MserialPort);
    ProcessOnCloseEvent();
end;

procedure SerialThread.SendString(Data: ansistring);
begin
    MserialPort.SendString(Data);
end;

function SerialThread.SendBuffer(buffer: pointer; length: integer): integer;
begin
    Result := MserialPort.SendBuffer(buffer, length);
end;

procedure SerialThread.ConfigureSerial(baud, bits: integer; parity: char;
    stop: integer; softflow, hardflow: boolean);
begin
    MserialPort.Config(baud, bits, parity, stop,
        softflow, hardflow);
end;

function SerialThread.isConnected(): boolean;
begin
    Result := ((MserialPort <> nil) and MserialPort.InstanceActive);
end;

procedure SerialThread.Execute;

begin
    MserialPort.Connect(MSerialName);
    if isConnected() then
    begin
        Synchronize(@ProcessConnectedEvent);
    end
    else
    begin
        Synchronize(@ProcessConnectedFailedEvent);
        Exit;
    end;
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

procedure SerialThread.ProcessConnectedEvent;
begin
    if Assigned(FOnConnected) then
    begin
        FOnConnected();
    end;
end;

procedure SerialThread.ProcessConnectedFailedEvent;
begin
    if Assigned(FOnConnectedFailed) then
    begin
        FOnConnectedFailed();
    end;
end;

procedure SerialThread.ProcessOnCloseEvent;
begin
    if Assigned(FOnClosed) then
    begin
        FOnClosed();
    end;
end;

end.
