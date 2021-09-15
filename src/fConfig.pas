{
  Main configuration form implemetation.
  See Simulator.dpr comments to uderstand how to interact with it.

  LICENSE:

  Copyright 2015-2021 Michal Petrilak, Jan Horacek

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
}

unit fConfig;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, IniFiles, Menus, StdCtrls, Spin;

const
  _DEFAULT_CFG_FILE = 'rcs/simcfg.ini'; // Config file. Change MTB ranges to make form more synoptic.
  _MAX_MTB = 255;
  _PINS = 16;

type
  TMyEvent = function(Sender: TObject): Integer of object; stdcall;
  TMyErrorEvent = function(Sender: TObject; errValue: word; errAddr: Cardinal; errStr: string)
    : Integer of object; stdcall;

  TModulType = (
    idNone = $0,
    idMTB_POT_ID = $10,
    idMTB_REGP_ID = $30,
    idMTB_UNI_ID = $40,
    idMTB_UNIOUT_ID = $50,
    idMTB_TTL_ID = $60,
    idMTB_TTLOUT_ID = $70
  );

  // Simulation status:
  TSimulatorStatus = (closed = 0, opening = 1, closing = 2, stopped = 3, starting = 4, running = 5, stopping = 6);

  TRCSIPortType = (iptPlain = 0, iptIR = 1);

  TRCSOPortType = (optPlain = 0, optSCom = 1);

  TModule = record
    name: string;
    typ: TModulType;
    fw: string;
    exists: boolean;
    failure: boolean;
    ir: Cardinal;
    scom: Cardinal;
  end;

  TFormConfig = class(TForm)
    GB_Error: TGroupBox;
    Label1: TLabel;
    SE_Err_id: TSpinEdit;
    Label2: TLabel;
    SE_Err_board: TSpinEdit;
    B_Error: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure B_ErrorClick(Sender: TObject);
    procedure SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
  private
    present: array [0 .. _MAX_MTB] of boolean;
    Cfgbtn: array [0 .. _MAX_MTB] of TButton; // configuration buttons
    pin: Array [0 .. _MAX_MTB, 0 .. _PINS - 1] of TShape; // pins (= ports)

    fstatus: TSimulatorStatus; // simulation status

    procedure ChangeInput(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ShowAddress(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure CfgBtnOnClick(Sender: TObject);
    procedure CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure SetStatus(new: TSimulatorStatus);
  public

    config_fn: string;

    procedure RePaintPins();

    procedure LoadData(filename: string);
    procedure SaveData(filename: string);
    procedure CreatePins();
    procedure FreePins();

    procedure OnOpen(Sender: TObject);
    procedure OnClose(Sender: TObject);
    procedure OnStart(Sender: TObject);
    procedure OnStop(Sender: TObject);

    procedure OnScanned(Sender: TObject);

    property Status: TSimulatorStatus read fstatus write SetStatus;
  end;

var
  FormConfig: TFormConfig;

var
  inputs: Array [0 .. _MAX_MTB, 0 .. _PINS - 1] of Byte; // input states
  outputs: Array [0 .. _MAX_MTB, 0 .. _PINS - 1] of Byte; // output states
  modules: array [0 .. _MAX_MTB] of TModule; // modules config
  api_version: Cardinal;

implementation

uses Board, LibraryEvents;

{$R *.dfm}

procedure TFormConfig.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := false;
  Hide;
end;

procedure TFormConfig.FormCreate(Sender: TObject);
var i: Integer;
begin
  for i := 0 to _MAX_MTB do
    Self.present[i] := false;

  Self.config_fn := _DEFAULT_CFG_FILE;
  try
    Self.LoadData(Self.config_fn);
  except

  end;

  Self.CreatePins();
  Self.Status := TSimulatorStatus.closed;
end;

procedure TFormConfig.FormDestroy(Sender: TObject);
begin
  try
    Self.SaveData(Self.config_fn);
    Self.FreePins();
  except

  end;
end;

procedure TFormConfig.CreatePins();
var module, i, port: Cardinal;
begin
  i := 0;
  for module := 0 to _MAX_MTB do
  begin
    if (not present[module]) then
    begin
      Cfgbtn[module] := nil;
      for port := 0 to _PINS - 1 do
        pin[module, port] := nil;
      continue;
    end;

    Cfgbtn[module] := TButton.Create(FormConfig);
    with (Cfgbtn[module]) do
    begin
      Parent := FormConfig;
      Top := 5;
      Left := i * 15 + 5;
      Caption := '?';
      Height := 25;
      Width := 13;
      Tag := module;
      OnClick := Self.CfgBtnOnClick;
      OnMouseMove := Self.CfgBtnOnMove;
    end;
    for port := 0 to _PINS - 1 do
    begin
      pin[module, port] := TShape.Create(FormConfig);
      with pin[module, port] do
      begin
        Parent := FormConfig;
        Left := i * 15 + 5;
        Top := port * 15 + 35;
        Width := 13;
        Height := 13;
        Pen.Width := 2;
        Tag := _PINS * module + port;
        OnMouseUp := ChangeInput;
        OnMouseMove := ShowAddress;
      end;
    end;
    Inc(i);
  end;
  Self.RePaintPins();

  Self.ClientWidth := (i * 15) + 8;
  Self.ClientHeight := (_PINS * 15) + 40 + GB_Error.Height + 2;
end;

procedure TFormConfig.FreePins();
var i, j: Integer;
begin
  for i := 0 to _MAX_MTB do
  begin
    present[i] := false;
    if (Assigned(Self.Cfgbtn[i])) then
      FreeAndNil(Self.Cfgbtn[i]);
    for j := 0 to _PINS - 1 do
      if (Assigned(Self.pin[i, j])) then
        FreeAndNil(Self.pin[i, j]);
  end;
end;

procedure TFormConfig.ChangeInput(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  module, port: Integer;
begin
  module := (Sender as TShape).Tag div _PINS;
  port := (Sender as TShape).Tag mod _PINS;

  inputs[module, port] := inputs[module, port] XOR 1;
  RePaintPins();
  if (Assigned(LibEvents.OnInputChanged.event)) then
    LibEvents.OnInputChanged.event(Self, LibEvents.OnInputChanged.data, module);
end;

procedure TFormConfig.RePaintPins();
var
  module, port: Integer;
  sh: TShape;
begin
  for module := 0 to _MAX_MTB do
  begin
    if (not present[module]) then
      continue;

    for port := 0 to _PINS - 1 do
    begin
      sh := pin[module, port];

      if ((modules[module].ir shr (port div 4)) and $1 > 0) then
        sh.Pen.Color := clFuchsia * inputs[module, port]
      else
        sh.Pen.Color := clRed * inputs[module, port];

      if ((modules[module].scom shr (port div 2)) and $1 > 0) then
      begin
        sh.Brush.Color := clAqua * Integer(outputs[module, port] > 0);
        sh.Hint := IntToStr(outputs[module, port]);
      end
      else
        sh.Brush.Color := clLime * Integer(outputs[module, port] > 0);
    end;
  end;
end;

procedure TFormConfig.ShowAddress(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  module, port: Integer;
begin
  module := (Sender as TShape).Tag div _PINS;
  port := (Sender as TShape).Tag mod _PINS;

  Caption := Format('%.3d / %.3d', [module, port])
end;

procedure TFormConfig.LoadData(filename: string);
var Ini: TMemIniFile;
  i: Integer;
  ranges_str, range_str: string;
  ranges, range: TStrings;
  start, finish: Integer;
begin
  Ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  ranges := TStringList.Create();
  range := TStringList.Create();
  try
    ranges_str := Ini.ReadString('MTB', 'ranges', '');
    if (ranges_str = '') then
    begin
      // backward compatibility
      ranges_str := Ini.ReadString('MTB', 'start', '0') + '-' + Ini.ReadString('MTB', 'end', '0');
      if (ranges_str = '0-0') then
        ranges_str := '0-31';
    end;

    ExtractStrings([','], [], PChar(ranges_str), ranges);
    for range_str in ranges do
    begin
      range.Clear();
      ExtractStrings(['-'], [], PChar(range_str), range);
      if (range.Count = 1) then
        present[StrToInt(range[0])] := true
      else if (range.Count = 2) then
      begin
        start := StrToInt(range[0]);
        if (start < 0) then
          start := 0;
        if (start > _MAX_MTB) then
          start := _MAX_MTB;

        finish := StrToInt(range[1]);
        if (finish < 0) then
          finish := 0;
        if (finish > _MAX_MTB) then
          finish := _MAX_MTB;

        for i := start to finish do
          present[i] := true;
      end;
    end;

    for i := 0 to _MAX_MTB do
    begin
      modules[i].name := Ini.ReadString('MTB' + IntToStr(i), 'name', 'Simulator' + IntToStr(i));
      modules[i].typ := TModulType(Ini.ReadInteger('MTB' + IntToStr(i), 'typ', Integer(idMTB_UNI_ID)));
      modules[i].fw := Ini.ReadString('MTB' + IntToStr(i), 'fw', 'VIRTUAL');
      modules[i].exists := Ini.ReadBool('MTB' + IntToStr(i), 'is', present[i]);
      modules[i].ir := Ini.ReadInteger('MTB' + IntToStr(i), 'ir', 0);
      modules[i].scom := Ini.ReadInteger('MTB' + IntToStr(i), 'scom', 0);
    end;
  finally
    Ini.Free();
    ranges.Free();
    range.Free();
  end;
end; // function

procedure TFormConfig.SaveData(filename: string);
var Ini: TMemIniFile;
  i: Integer;
begin
  Ini := TMemIniFile.Create(filename, TEncoding.UTF8);
  try
    for i := 0 to _MAX_MTB do
    begin
      if (modules[i].name <> '') and (modules[i].name <> 'Simulator' + IntToStr(i)) then
        Ini.WriteString('MTB' + IntToStr(i), 'name', modules[i].name);
      if (modules[i].typ <> idMTB_UNI_ID) then
        Ini.WriteInteger('MTB' + IntToStr(i), 'typ', Integer(modules[i].typ));
      if (modules[i].fw <> 'VIRTUAL') then
        Ini.WriteString('MTB' + IntToStr(i), 'fw', modules[i].fw);
      if (modules[i].exists) then
        Ini.WriteBool('MTB' + IntToStr(i), 'is', modules[i].exists);
      if (modules[i].ir <> 0) then
        Ini.WriteInteger('MTB' + IntToStr(i), 'ir', modules[i].ir);
      if (modules[i].scom <> 0) then
        Ini.WriteInteger('MTB' + IntToStr(i), 'scom', modules[i].scom);
    end;
  finally
    Ini.UpdateFile();
    Ini.Free();
  end;
end;

procedure TFormConfig.SE_Err_boardKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Self.B_ErrorClick(Self);
end; // procedure

procedure TFormConfig.B_ErrorClick(Sender: TObject);
begin
  if (Assigned(LibEvents.OnError.event)) then
    LibEvents.OnError.event(Self, LibEvents.OnError.data, Self.SE_Err_id.Value, Self.SE_Err_board.Value, '');
end;

procedure TFormConfig.CfgBtnOnClick(Sender: TObject);
begin
  F_Board.OpenForm((Sender as TButton).Tag);
end;

procedure TFormConfig.CfgBtnOnMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Caption := Format('%.3d', [(Sender as TButton).Tag]);
end;

procedure TFormConfig.SetStatus(new: TSimulatorStatus);
begin
  case (new) of
    TSimulatorStatus.closed:
      Self.Caption := 'Closed';
    TSimulatorStatus.opening:
      Self.Caption := 'Opening...';
    TSimulatorStatus.stopped:
      Self.Caption := 'Openned, stopped';
    TSimulatorStatus.starting:
      Self.Caption := 'Starting...';
    TSimulatorStatus.running:
      Self.Caption := 'Running';
    TSimulatorStatus.stopping:
      Self.Caption := 'Stopping...';
    TSimulatorStatus.closing:
      Self.Caption := 'Closing...';
  end; // case

  Self.fstatus := new;
end;

/// /////////////////////////////////////////////////////////////////////////////
// events from simulator timer:

procedure TFormConfig.OnOpen(Sender: TObject);
begin
  (Sender as TTimer).Enabled := false;
  Status := TSimulatorStatus.stopped;
  if (Assigned(LibEvents.AfterOpen.event)) then
    LibEvents.AfterOpen.event(FormConfig, LibEvents.AfterOpen.data);
end;

procedure TFormConfig.OnClose(Sender: TObject);
begin
  (Sender as TTimer).Enabled := false;
  Status := TSimulatorStatus.closed;
  if (Assigned(LibEvents.AfterClose.event)) then
    LibEvents.AfterClose.event(FormConfig, LibEvents.AfterClose.data);
end;

procedure TFormConfig.OnStart(Sender: TObject);
begin
  Status := TSimulatorStatus.running;

  if (F_Board.Showing) then
    F_Board.RG_Failure.Enabled := modules[F_Board.OpenIndex].exists;

  if (Assigned(LibEvents.AfterStart.event)) then
    LibEvents.AfterStart.event(FormConfig, LibEvents.AfterStart.data);

  (Sender as TTimer).Interval := 500;
  (Sender as TTimer).OnTimer := Self.OnScanned;
end;

procedure TFormConfig.OnStop(Sender: TObject);
begin
  (Sender as TTimer).Enabled := false;
  Status := TSimulatorStatus.stopped;
  F_Board.RG_Failure.Enabled := false;
  F_Board.RG_Exists.Enabled := true;
  if (Assigned(LibEvents.AfterStop.event)) then
    LibEvents.AfterStop.event(FormConfig, LibEvents.AfterStop.data);
end;

procedure TFormConfig.OnScanned(Sender: TObject);
begin
  (Sender as TTimer).Enabled := false;
  if (Assigned(LibEvents.OnScanned.event)) then
    LibEvents.OnScanned.event(FormConfig, LibEvents.OnScanned.data);
end;

/// /////////////////////////////////////////////////////////////////////////////

initialization

finalization

FreeAndNil(FormConfig);

end.
