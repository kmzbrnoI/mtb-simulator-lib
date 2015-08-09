unit LibraryEvents;

interface

type
  TMyErrorEvent = function (Sender: TObject; errValue: word; errAddr: byte; errMsg:string):Integer; stdcall;
  TStdNotifyEvent = procedure (Sender: TObject) of object; stdcall;
  TMyModuleChangeEvent = procedure (Sender: TObject; module: byte) of object; stdcall;

  TLibEvents = record
    BeforeOpen:TStdNotifyEvent;
    AfterOpen:TStdNotifyEvent;
    BeforeClose:TStdNotifyEvent;
    AfterClose:TStdNotifyEvent;

    BeforeStart:TStdNotifyEvent;
    AfterStart:TStdNotifyEvent;
    BeforeStop:TStdNotifyEvent;
    AfterStop:TStdNotifyEvent;

    OnError:TMyErrorEvent;
    OnInputChanged:TMyModuleChangeEvent;
    OnOutputChanged:TMyModuleChangeEvent;
  end;

var
   LibEvents:TLibEvents;

implementation

end.

