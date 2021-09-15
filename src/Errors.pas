{
  Definition of error codes.

  LICENSE:

  Copyright 2016-2021 Jan Horacek

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

unit Errors;

interface

const
  RCS_GENERAL_EXCEPTION = 1000;
  RCS_FILE_CANNOT_ACCESS = 1010;
  RCS_FILE_DEVICE_OPENED = 1011;

  RCS_MODULE_INVALID_ADDR = 1100;
  RCS_PORT_INVALID_NUMBER = 1103;
  RCS_PORT_INVALID_VALUE = 1106;
  RCS_INPUT_NOT_YET_SCANNED = 1108;

  RCS_ALREADY_OPENNED = 2001;
  RCS_CANNOT_OPEN_PORT = 2002;
  RCS_FIRMWARE_TOO_LOW = 2003;
  RCS_DEVICE_DISCONNECTED = 2004;
  RCS_SCANNING_NOT_FINISHED = 2010;
  RCS_NOT_OPENED = 2011;
  RCS_ALREADY_STARTED = 2012;
  RCS_OPENING_NOT_FINISHED = 2021;
  RCS_NO_MODULES = 2025;
  RCS_NOT_STARTED = 2031;

  RCS_MODULE_FAILED = 3141;
  RCS_MODULE_RESTORED = 3142;

  RCS_UNSUPPORTED_API_VERSION = 4000;

implementation

end.
