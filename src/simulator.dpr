{
  Main library file.

  This file provides outer interface of the library, almost all functions
  are called to TForm_config (there is no technology class in the project).

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

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
library simulator;

uses
  SysUtils,
  ExtCtrls,
  Math,
  Windows,
  Forms,
  Classes,
  IniFiles,
  fConfig in 'fConfig.pas' {FormConfig} ,
  Board in 'Board.pas' {F_Board} ,
  LibraryEvents in 'LibraryEvents.pas',
  Errors in 'Errors.pas';

{$R *.res}

const
  // v1.4, v1.5
  API_SUPPORTED_VERSIONS: array [0 .. 1] of Cardinal = ($0401, $0501);

type
  TAddr = 0 .. 255;

var
  // this timer simulates delays on open, close, start, stop
  t_event: TTimer;

/// /////////////////////////////////////////////////////////////////////////////
// Simple timer activation function:

procedure ActivateTimer(callback: TNotifyEvent; interval: Integer);
begin
  t_event.OnTimer := callback;
  t_event.interval := interval;
  t_event.Enabled := true;
end;

/// /////////////////////////////////////////////////////////////////////////////
// configurations files load/save

function LoadConfig(filename: PChar): Integer; stdcall;
begin
  if (FormConfig.Status > TSimulatorStatus.closed) then
    Exit(RCS_FILE_DEVICE_OPENED);

  try
    FormConfig.FreePins();
    FormConfig.LoadData(filename);
    FormConfig.CreatePins();
    FormConfig.config_fn := filename;
    Result := 0;
  except
    on E: EIniFileException do
      Result := RCS_FILE_CANNOT_ACCESS;
    on E: Exception do
      Result := RCS_GENERAL_EXCEPTION;
  end;
end;

function SaveConfig(filename: PChar): Integer; stdcall;
begin
  try
    FormConfig.SaveData(filename);
    Result := 0;
  except
    on E: EIniFileException do
      Result := RCS_FILE_CANNOT_ACCESS;
    on E: Exception do
      Result := RCS_GENERAL_EXCEPTION;
  end;
end;

procedure SetConfigFileName(filename: PChar); stdcall;
begin
  try
    FormConfig.config_fn := filename;
  except

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// logging

procedure SetLogLevel(loglevel: Cardinal); stdcall;
begin

end;

function GetLogLevel(): Cardinal; stdcall;
begin
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure ShowConfigDialog(); stdcall;
begin
  try
    FormConfig.Show();
  finally

  end;
end;

procedure HideConfigDialog(); stdcall;
begin
  try
    FormConfig.Hide();
  finally

  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Open MTB, start scanning

function Open(): Integer; stdcall;
begin
  if (FormConfig.Status > TSimulatorStatus.closed) then
    Exit(RCS_ALREADY_OPENNED);

  for var i := 0 to _MAX_MTB do
    modules[i].failure := false;
  F_Board.CHB_failure.Checked := false;

  try
    if (Assigned(LibEvents.BeforeOpen.event)) then
      LibEvents.BeforeOpen.event(FormConfig, LibEvents.BeforeOpen.data);
    FormConfig.Status := TSimulatorStatus.opening;
    ActivateTimer(FormConfig.OnOpen, 1500);
    Result := 0;
  except
    Result := RCS_GENERAL_EXCEPTION;
  end;
end;

function OpenDevice(device: PChar; persist: boolean): Integer; stdcall;
begin
  Result := Open();
end;

/// /////////////////////////////////////////////////////////////////////////////
// Close MTB

function Close(): Integer; stdcall;
begin
  if (FormConfig.Status < TSimulatorStatus.stopped) then
    Exit(RCS_NOT_OPENED);

  if (FormConfig.Status = TSimulatorStatus.starting) then
    Exit(RCS_SCANNING_NOT_FINISHED);

  try
    if (Assigned(LibEvents.BeforeClose.event)) then
      LibEvents.BeforeClose.event(FormConfig, LibEvents.BeforeClose.data);
    FormConfig.Status := TSimulatorStatus.closing;
    ActivateTimer(FormConfig.OnClose, 500);
    Result := 0;
  except
    Result := RCS_GENERAL_EXCEPTION;
  end;
end;

function Opened(): boolean; stdcall;
begin
  Result := (FormConfig.Status >= TSimulatorStatus.stopped)
end;

/// /////////////////////////////////////////////////////////////////////////////
// Start communication

function Start(): Integer; stdcall;
begin
  if (FormConfig.Status > TSimulatorStatus.stopped) then
    Exit(RCS_ALREADY_STARTED);

  var cnt := 0;
  for var i := 0 to _MAX_MTB do
    if (modules[i].exists) then
      Inc(cnt);

  if (cnt = 0) then
    Exit(RCS_NO_MODULES);

  if (FormConfig.Status = TSimulatorStatus.opening) then
    Exit(RCS_SCANNING_NOT_FINISHED);

  if (FormConfig.Status < TSimulatorStatus.stopped) then
    Exit(RCS_NOT_OPENED);

  try
    if (Assigned(LibEvents.BeforeStart.event)) then
      LibEvents.BeforeStart.event(FormConfig, LibEvents.BeforeStart.data);
    FormConfig.Status := TSimulatorStatus.starting;

    ActivateTimer(FormConfig.OnStart, 500);
    Result := 0;
  except
    Result := RCS_GENERAL_EXCEPTION;
  end;
end;

/// /////////////////////////////////////////////////////////////////////////////
// Stop communication

function Stop(): Integer; stdcall;
begin
  if (FormConfig.Status < TSimulatorStatus.running) then
    Exit(RCS_NOT_STARTED);

  try
    if (Assigned(LibEvents.BeforeStop.event)) then
      LibEvents.BeforeStop.event(FormConfig, LibEvents.BeforeStop.data);
    FormConfig.Status := TSimulatorStatus.stopping;
    ActivateTimer(FormConfig.OnStop, 500);
    Result := 0;
  except
    Result := RCS_GENERAL_EXCEPTION;
  end;
end;

function Started(): boolean; stdcall;
begin
  Result := (FormConfig.Status = TSimulatorStatus.running);
end;

/// /////////////////////////////////////////////////////////////////////////////

function GetInput(module, port: Cardinal): Integer; stdcall;
begin
  if (FormConfig.Status = TSimulatorStatus.starting) then
    Exit(RCS_INPUT_NOT_YET_SCANNED);
  if (FormConfig.Status <> TSimulatorStatus.running) then
    Exit(RCS_NOT_STARTED);
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);
  if (modules[module].failure) then
    Exit(RCS_MODULE_FAILED);

  Result := inputs[module, port];
end;

function SetOutput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  if (FormConfig.Status <> TSimulatorStatus.running) then
    Exit(RCS_NOT_STARTED);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);
  if (modules[module].failure) then
    Exit(RCS_MODULE_FAILED);
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);
  if (outputs[module, port] = state) then
    Exit(0);

  outputs[module, port] := state;
  FormConfig.RepaintPin(module, port);
  if (Assigned(LibEvents.OnOutputChanged.event)) then
    LibEvents.OnOutputChanged.event(FormConfig, LibEvents.OnOutputChanged.data, module);
  Result := 0;
end;

function GetOutput(module, port: Cardinal): Integer; stdcall;
begin
  if (FormConfig.Status <> TSimulatorStatus.running) then
    Exit(RCS_NOT_STARTED);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);
  if (modules[module].failure) then
    Exit(RCS_MODULE_FAILED);
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);

  Result := outputs[module, port];
end;

function GetInputType(module, port: Cardinal): Integer; stdcall;
begin
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);

  if ((modules[module].irs shr port) and $1 > 0) then
    Result := Integer(TRCSIPortType.iptIR)
  else
    Result := Integer(TRCSIPortType.iptPlain);
end;

function GetOutputType(module, port: Cardinal): Integer; stdcall;
begin
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);

  if ((modules[module].scoms shr port) and $1 > 0) then
    Result := Integer(TRCSOPortType.optSCom)
  else
    Result := Integer(TRCSOPortType.optPlain);
end;

/// /////////////////////////////////////////////////////////////////////////////
// Set input:
// -- special function just for simulator purposes --

function SetInput(module, port: Cardinal; state: Integer): Integer; stdcall;
begin
  if (port >= _PINS) then
    Exit(RCS_PORT_INVALID_NUMBER);
  if ((not InRange(module, Low(TAddr), High(TAddr))) or (not modules[module].exists)) then
    Exit(RCS_MODULE_INVALID_ADDR);

  inputs[module, port] := state;
  if (Assigned(FormConfig)) then
    FormConfig.RepaintPin(module, port);
  Result := 0;
end;

/// /////////////////////////////////////////////////////////////////////////////

function GetDeviceCount(): Integer; stdcall;
begin
  Result := 1;
end;

procedure GetDeviceSerial(index: Integer; serial: PChar; serialLen: Cardinal); stdcall;
begin
  if (index = 0) then
    StrPLCopy(serial, 'SIM DEVICE', serialLen);
end;

/// /////////////////////////////////////////////////////////////////////////////

function IsModule(module: Cardinal): boolean; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(false);

  if (FormConfig.Status >= TSimulatorStatus.stopped) then
    Result := modules[module].exists
  else
    Result := false;
end;

function IsModuleFailure(module: Cardinal): boolean; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(false);

  if (FormConfig.Status >= TSimulatorStatus.stopped) then
    Result := modules[module].failure
  else
    Result := false;
end;

function IsModuleWarning(module: Cardinal): boolean; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(false);

  if (FormConfig.Status >= TSimulatorStatus.stopped) then
    Result := modules[module].warning
  else
    Result := false;
end;

function IsModuleError(module: Cardinal): boolean; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(false);

  if (FormConfig.Status >= TSimulatorStatus.stopped) then
    Result := modules[module].error
  else
    Result := false;
end;

function GetModuleCount(): Cardinal; stdcall;
var cnt: Cardinal;
begin
  cnt := 0;

  for var i := 0 to _MAX_MTB do
    if (modules[i].exists) then
      Inc(cnt);

  Result := cnt;
end;

function GetMaxModuleAddr(): Cardinal; stdcall;
begin
  Result := _MAX_MTB;
end;

function GetModuleTypeStr(module: Cardinal; typ: PChar; maxTypeLen: Cardinal): Integer; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(RCS_MODULE_INVALID_ADDR);

  StrPLCopy(typ, modules[module].typ, maxTypeLen);
  Result := 0;
end;

function GetModuleName(module: Cardinal; name: PChar; nameLen: Cardinal): Integer; stdcall;
begin
  if (module <= 255) then
  begin
    StrPLCopy(name, modules[module].name, nameLen);
    Result := 0;
  end
  else
    Result := RCS_MODULE_INVALID_ADDR;
end;

function GetModuleFW(module: Cardinal; fw: PChar; fwLen: Cardinal): Integer; stdcall;
begin
  if (not InRange(module, Low(TAddr), High(TAddr))) then
    Exit(RCS_MODULE_INVALID_ADDR);

  if (FormConfig.Status >= TSimulatorStatus.stopped) then
  begin
    StrPLCopy(fw, modules[module].fw, fwLen);
    Result := 0;
  end
  else
    Result := RCS_MODULE_INVALID_ADDR;
end;

function GetModuleInputsCount(module: Cardinal): Cardinal; stdcall;
begin
  if (module > _MAX_MTB) then
    Exit(RCS_MODULE_INVALID_ADDR);
  Result := _PINS;
end;

function GetModuleOutputsCount(module: Cardinal): Cardinal; stdcall;
begin
  if (module > _MAX_MTB) then
    Exit(RCS_MODULE_INVALID_ADDR);
  Result := _PINS;
end;

/// /////////////////////////////////////////////////////////////////////////////

function ApiSupportsVersion(version: Cardinal): boolean; stdcall;
begin
  for var i := Low(API_SUPPORTED_VERSIONS) to High(API_SUPPORTED_VERSIONS) do
    if (API_SUPPORTED_VERSIONS[i] = version) then
      Exit(true);
  Result := false;
end;

function ApiSetVersion(version: Cardinal): Integer; stdcall;
begin
  if (not ApiSupportsVersion(version)) then
    Exit(RCS_UNSUPPORTED_API_VERSION);

  api_version := version;
  Result := 0;
end;

function GetDeviceVersion(version: PChar; versionLen: Cardinal): Integer; stdcall;
begin
  if (FormConfig.Status >= TSimulatorStatus.stopped) then
  begin
    StrPLCopy(version, 'MTB-SIMULATOR-V', versionLen);
    Result := 0;
  end
  else
    Result := RCS_DEVICE_DISCONNECTED;
end;

procedure GetDriverVersion(version: PChar; versionLen: Cardinal); stdcall;
begin
  StrPLCopy(version, 'SIMULATOR-DRIVER-V', versionLen);
end;

/// /////////////////////////////////////////////////////////////////////////////
// ----- setting events begin -----

procedure BindBeforeOpen(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.BeforeOpen.data := data;
  LibEvents.BeforeOpen.event := event;
end;

procedure BindAfterOpen(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.AfterOpen.data := data;
  LibEvents.AfterOpen.event := event;
end;

procedure BindBeforeClose(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.BeforeClose.data := data;
  LibEvents.BeforeClose.event := event;
end;

procedure BindAfterClose(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.AfterClose.data := data;
  LibEvents.AfterClose.event := event;
end;

procedure BindBeforeStart(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.BeforeStart.data := data;
  LibEvents.BeforeStart.event := event;
end;

procedure BindAfterStart(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.AfterStart.data := data;
  LibEvents.AfterStart.event := event;
end;

procedure BindBeforeStop(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.BeforeStop.data := data;
  LibEvents.BeforeStop.event := event;
end;

procedure BindAfterStop(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.AfterStop.data := data;
  LibEvents.AfterStop.event := event;
end;

procedure BindOnError(event: TStdErrorEvent; data: Pointer); stdcall;
begin
  LibEvents.OnError.data := data;
  LibEvents.OnError.event := event;
end;

procedure BindOnLog(event: TStdLogEvent; data: Pointer); stdcall;
begin
  // no logging messages here
end;

procedure BindOnInputChanged(event: TStdModuleChangeEvent; data: Pointer); stdcall;
begin
  LibEvents.OnInputChanged.data := data;
  LibEvents.OnInputChanged.event := event;
end;

procedure BindOnOutputChanged(event: TStdModuleChangeEvent; data: Pointer); stdcall;
begin
  LibEvents.OnOutputChanged.data := data;
  LibEvents.OnOutputChanged.event := event;
end;

procedure BindOnModuleChanged(event: TStdModuleChangeEvent; data: Pointer); stdcall;
begin
  LibEvents.OnModuleChanged.data := data;
  LibEvents.OnModuleChanged.event := event;
end;

procedure BindOnScanned(event: TStdNotifyEvent; data: Pointer); stdcall;
begin
  LibEvents.OnScanned.data := data;
  LibEvents.OnScanned.event := event;
end;

// ----- setting events end -----
/// /////////////////////////////////////////////////////////////////////////////

function IsSimulation(): boolean; stdcall;
begin
  Result := true;
end;

/// /////////////////////////////////////////////////////////////////////////////

// Exported functions:

exports
  LoadConfig, SaveConfig, SetConfigFileName,
  SetLogLevel, GetLogLevel,
  ShowConfigDialog, HideConfigDialog,
  Open, OpenDevice, Close, Opened, Start, Stop, Started,
  GetInput, GetOutput, SetOutput,
  GetDeviceCount, GetDeviceSerial,
  IsModule, IsModuleFailure, IsModuleWarning, IsModuleError,
  GetModuleCount, GetMaxModuleAddr, GetModuleTypeStr,
  GetModuleName, GetModuleFW, GetModuleInputsCount, GetModuleOutputsCount,
  ApiSupportsVersion, ApiSetVersion, GetDeviceVersion, GetDriverVersion,
  BindBeforeOpen, BindAfterOpen, BindBeforeClose, BindAfterClose,
  BindBeforeStart, BindAfterStart, BindBeforeStop, BindAfterStop,
  BindOnError, BindOnLog, BindOnInputChanged, BindOnOutputChanged, BindOnModuleChanged,
  BindOnScanned, SetInput, GetInputType, GetOutputType,
  IsSimulation;

begin
  Application.CreateForm(TFormConfig, FormConfig);
  Application.CreateForm(TF_Board, F_Board);
  t_event := TTimer.Create(FormConfig);
  t_event.Enabled := false;

end.
