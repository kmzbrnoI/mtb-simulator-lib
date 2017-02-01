unit LibraryEvents;

interface

type
  TStdNotifyEvent = procedure (Sender: TObject; data:Pointer); stdcall;
  TStdLogEvent = procedure (Sender: TObject; data:Pointer; logLevel:Integer; msg:PChar); stdcall;
  TStdErrorEvent = procedure (Sender: TObject; data:Pointer; errValue: word; errAddr: byte; errMsg:PChar); stdcall;
  TStdModuleChangeEvent = procedure (Sender: TObject; data:Pointer; module: byte); stdcall;

  TMyErrorEvent = record
    event: TStdErrorEvent;
    data: Pointer;
  end;
  TMyNotifyEvent = record
    event: TStdNotifyEvent;
    data: Pointer;
  end;
  TMyModuleChangeEvent = record
    event: TStdModuleChangeEvent;
    data:Pointer
  end;

  TLibEvents = record
    BeforeOpen:TMyNotifyEvent;
    AfterOpen:TMyNotifyEvent;
    BeforeClose:TMyNotifyEvent;
    AfterClose:TMyNotifyEvent;

    BeforeStart:TMyNotifyEvent;
    AfterStart:TMyNotifyEvent;
    BeforeStop:TMyNotifyEvent;
    AfterStop:TMyNotifyEvent;

    OnError:TMyErrorEvent;
    OnInputChanged:TMyModuleChangeEvent;
    OnOutputChanged:TMyModuleChangeEvent;
  end;

var
   LibEvents:TLibEvents;

implementation

end.
