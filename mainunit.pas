unit mainunit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
    ComCtrls, Spin, SpinEx, Synaser, SerialManager, Types;

type

    { TMainForm }

    TMainForm = class(TForm)
        baudrateComboBox: TComboBox;
        TimerCheckBox: TCheckBox;
        closeButton: TButton;
        MSLabel: TLabel;
        RecvGroupBox: TGroupBox;
        SendEncodingComboBox: TComboBox;
        RecvEncodingComboBox: TComboBox;
        RecvEncodingLabel: TLabel;
        SerialSendGroupBox: TGroupBox;
        SendEncodingLabel: TLabel;
        MainPageControl: TPageControl;
        SerialGroupBox: TGroupBox;
        RecvMemo: TMemo;
        openButton: TButton;
        databitComboBox: TComboBox;
        databitLabel: TLabel;
        SendMemo: TMemo;
        sendButton: TButton;
        ClearButton: TButton;
        SerialSendTimer: TTimer;
        TimerSpinEdit: TSpinEdit;
        stopbitComboBox: TComboBox;
        baudrateLabel: TLabel;
        paritybitComboBox: TComboBox;
        serialComboBox: TComboBox;
        stopbitLabel: TLabel;
        paritybitLabel: TLabel;
        serialLabel: TLabel;
        SerialListTimer: TTimer;
        SerialTabSheet: TTabSheet;
        TabSheet2: TTabSheet;

        procedure ClearButtonClick(Sender: TObject);
        procedure closeButtonClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure openButtonClick(Sender: TObject);
        procedure sendButtonClick(Sender: TObject);
        procedure SerialListTimerTimer(Sender: TObject);
        procedure SerialSendTimerTimer(Sender: TObject);
        procedure TimerCheckBoxChange(Sender: TObject);
        procedure TimerSpinEditChange(Sender: TObject);
    private
        serialThr: SerialThread;
        serialPort: TBlockSerial;
        function isSerialConnected(): boolean;
        procedure closeSerial();
        procedure processReceivedSerialData(Data: string);
        procedure showWarnBox(message: string);
    public
        constructor Create(TheOwner: TComponent); override;
    end;

var
    MainForm: TMainForm;


implementation

{$R *.lfm}

{ TMainForm }



procedure TMainForm.ClearButtonClick(Sender: TObject);
begin
    RecvMemo.Clear();

end;

procedure TMainForm.closeButtonClick(Sender: TObject);
begin
    closeSerial();
    openButton.Enabled := True;
    serialComboBox.Enabled := True;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    closeSerial();
    FreeAndNil(serialPort);
end;



procedure TMainForm.openButtonClick(Sender: TObject);
var
    baudRate: integer;
    databit: integer;
    paritybit: char;
    stopbit: integer;
begin

    if TryStrToInt(baudrateComboBox.Text, baudRate) then
        baudrate := StrToInt(baudrateComboBox.Text)
    else
    begin
        showWarnBox('波特率参数错误');
        Exit;
    end;

    databit := StrToInt(databitComboBox.Text);

    case (paritybitComboBox.Text) of
        '无': paritybit := 'N';
        '奇校验': paritybit := 'O';
        '偶校验': paritybit := 'E';
        else
            paritybit := 'N';
    end;

    case (stopbitComboBox.Text) of
        '1': stopbit := SB1;
        '1.5': stopbit := SB1andHalf;
        '2': stopbit := SB2;
        //else
        //  stopbit := SB1;
    end;

    openButton.Enabled := False; // 防止多次点击
    serialPort.Connect(serialComboBox.Text);
    if isSerialConnected() then
    begin
        serialPort.Config(baudrate, databit, paritybit, stopbit,
            False, False);
        serialThr := SerialThread.Create(True, serialPort);
        serialThr.OnRecvData := @processReceivedSerialData;
        serialThr.Start;
        serialComboBox.Enabled := False;
        openButton.Enabled := False;
    end
    else
    begin
        showWarnBox('串口打开失败');
        openButton.Enabled := True;
        closeButton.Enabled := False;
    end;
end;

procedure TMainForm.sendButtonClick(Sender: TObject);
var
    hexArr: array of char;
    binBufferLen: integer;
begin
    if isSerialConnected and (SendMemo.GetTextLen > 0) then
    begin
        if SendEncodingComboBox.ItemIndex = 0 then
            serialPort.SendString(SendMemo.Text)
        else
        begin
            SetLength(hexArr, SendMemo.GetTextLen div 2);
            binBufferLen := HexToBin(PChar(SendMemo.Text), PChar(hexArr),
                SendMemo.GetTextLen div 2);
            serialPort.SendBuffer(PChar(hexArr), binBufferLen);
        end;
    end;
end;

procedure TMainForm.SerialListTimerTimer(Sender: TObject);
var
    serialList: string;
begin
    // 刷新串口列表
    serialList := GetSerialPortNames;
    if (serialList.IsEmpty) then
    begin
        // 只在当前Combox非空时Clear，防止闪烁
        if not (serialComboBox.Items.CommaText.IsEmpty) then
        begin
            serialComboBox.Items.Clear;
        end;
        openButton.Enabled := False;
    end
    else
    begin
        if not (serialComboBox.Items.CommaText.Equals(serialList)) then
        begin
            serialComboBox.Items.CommaText := serialList;
            serialComboBox.ItemIndex := 0;
        end;
        openButton.Enabled := True;
    end;
    // 设置按钮状态
    if isSerialConnected() then
    begin
        openButton.Enabled := False;
        sendButton.Enabled := True;
        closeButton.Enabled := True;
    end
    else
    begin
        sendButton.Enabled := False;
        closeButton.Enabled := False;
    end;
end;

procedure TMainForm.SerialSendTimerTimer(Sender: TObject);
begin
    sendButtonClick(Sender);
end;

procedure TMainForm.TimerCheckBoxChange(Sender: TObject);
begin
    if (Sender is TCheckBox) then
    begin
        if (Sender as TCheckBox).Checked then
        begin
            SerialSendTimer.Enabled := True;
        end
        else
        begin
            SerialSendTimer.Enabled := False;
        end;
    end;
end;

procedure TMainForm.TimerSpinEditChange(Sender: TObject);
begin
    SerialSendTimer.Interval := TimerSpinEdit.Value;
end;


function TMainForm.isSerialConnected: boolean;
begin
    Result := ((serialPort <> nil) and serialPort.InstanceActive);
end;

procedure TMainForm.closeSerial;
begin
    if isSerialConnected() then
    begin
        closeButton.Enabled := False;
        serialPort.Purge();
        serialPort.CloseSocket();
        serialThr.Terminate();
    end;
end;

procedure TMainForm.processReceivedSerialData(Data: string);
var
    hexArr: array of char;
    hexResult: string;
    i: integer;
begin
    RecvMemo.SelStart := Length(RecvMemo.Text);
    if RecvEncodingComboBox.ItemIndex = 0 then
    begin
        { TODO -omycai : 将各类换行符换成LineBreak }
        RecvMemo.SelText := Data;
    end
    else
    begin
        hexResult := '';
        SetLength(hexArr, Data.Length * 2);
        BinToHex(Pointer(Data), PChar(hexArr), Data.Length);
        for i := 0 to High(hexArr) div 2 do
            hexResult := hexResult + hexArr[i * 2] + hexArr[i * 2 + 1] + ':';
        RecvMemo.SelText := ansistring(hexResult);
    end;
end;

procedure TMainForm.showWarnBox(message: string);
var
    box: TForm;
begin
    box := CreateMessageDialog(message, mtWarning, [mbClose]);
    box.Position := poOwnerFormCenter;
    box.ShowModal;
end;

constructor TMainForm.Create(TheOwner: TComponent);
begin
    inherited Create(TheOwner);
    serialPort := TBlockSerial.Create;
    // 接收到\n即换行
    RecvMemo.Lines.TextLineBreakStyle := tlbsLF;
end;

end.
