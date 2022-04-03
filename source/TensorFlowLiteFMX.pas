unit TensorFlowLiteFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, FMX.Types, System.IOUtils,

{$IFDEF ANDROID}

{$ENDIF ANDROID}
{$IFDEF MSWINDOWS}
  WinApi.Windows
{$ENDIF MSWINDOWS}
    ;

{$IFDEF ANDROID}


const
  // LibraryName = 'tflite.so';
{$ENDIF ANDROID}
{$IFDEF MSWINDOWS}

const
  LibraryName = 'tflite.dll'; // TensorFlow Lite v2.7.0
{$ENDIF MSWINDOWS}


var
  LibraryModule: HMODULE = 0;

type
  TFLiteStatus = (TFLiteOk, TFLiteError, TFLiteDelegateError);

  TFLiteType = (TFLiteNoType = 0, TFLiteFloat32 = 1, TFLiteInt32 = 2,
    TFLiteUInt8 = 3, TFLiteInt64 = 4, TFLiteString = 5, TFLiteBool = 6,
    TFLiteInt16 = 7, TFLiteComplex64 = 8, TFLiteInt8 = 9,
    TFLiteFloat16 = 10, TFLiteFloat64 = 11);

type
  TTensorFlowLiteFMXTensor = record
    Tensor: Pointer;
    Name: String;
    TensorType: TFLiteType;
    Data: Pointer;
    DataSize: Int32;
    NumberDimensions: Int32;
  end;

type
  TTensorFlowLiteFMXTensors = record
    Tensors: array of TTensorFlowLiteFMXTensor;
    Count: Int32;
  end;

type
  ETensorFlowLiteFMXError = class(Exception);

  TTensorFlowLiteFMX = class(TComponent)
  private

  public
    InterpreterOptions: Pointer;
    Model: Pointer;
    Interpreter: Pointer;

    ModelFileName: String;

    Input: TTensorFlowLiteFMXTensors;
    Output: TTensorFlowLiteFMXTensors;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function LoadModel(ModelPath: String; InterpreterThreadCount: Integer): TFLiteStatus;
    procedure UnloadModel;

    function GetTensorSize(Tensor: Pointer): NativeUInt;

    function GetInputTensor(InputIndex: Int32): Pointer;
    function GetOutputTensor(OutputIndex: Int32): Pointer;

    function SetInputData(InputTensor: Int32; InputData: Pointer; InputDataSize: NativeUInt): TFLiteStatus;
    function GetOutputData(OutputTensor: Int32; OutputData: Pointer; OutputDataSize: NativeUInt): TFLiteStatus;

    function Inference(): TFLiteStatus;
  end;

implementation

type
  TFLiteModelCreateFromFile = function(const ModelPath: {$IFDEF MSWINDOWS}PAnsiChar{$ENDIF MSWINDOWS} {$IFDEF ANDROID}Pointer{$ENDIF ANDROID}): Pointer; stdcall;
  TFLiteInterpreterCreate = function(Model: Pointer; OptionalOptions: Pointer): Pointer; stdcall;
  TFLiteInterpreterOptionsCreate = function(): Pointer; stdcall;
  TfLiteInterpreterDelete = procedure(Interpreter: Pointer); stdcall;
  TFLiteInterpreterAllocateTensors = function(Interpreter: Pointer): TFLiteStatus; stdcall;
  TFLiteInterpreterGetInputTensor = function(Interpreter: Pointer; input_index: Int32): Pointer; stdcall;
  TFLiteInterpreterGetOutputTensor = function(Interpreter: Pointer; output_index: Int32): Pointer; stdcall;
  TFLiteTensorNumDims = function(Tensor: Pointer): Int32; stdcall;
  TFLiteTensorName = function(Tensor: {$IFDEF MSWINDOWS}Pointer{$ENDIF MSWINDOWS} {$IFDEF ANDROID}Pointer{$ENDIF ANDROID}): Pointer; stdcall;
  TFLiteTensorType = function(Tensor: Pointer): TFLiteType; stdcall;
  TFLiteTensorByteSize = function(Tensor: Pointer): NativeUInt; stdcall;

  TFLiteInterpreterGetInputTensorCount = function(Interpreter: Pointer): Int32; stdcall;
  TFLiteInterpreterGetOutputTensorCount = function(Interpreter: Pointer): Int32; stdcall;
  TFLiteTensorCopyFromBuffer = function(Tensor: Pointer; input_data: Pointer; input_data_size: NativeUInt): TFLiteStatus; stdcall;
  TFLiteTensorCopyToBuffer = function(output_tensor: Pointer; output_data: Pointer; output_data_size: NativeUInt): TFLiteStatus; stdcall;
  TFLiteInterpreterOptionsSetNumThreads = procedure(options: Pointer; num_threads: Int32); stdcall;
  TFLiteInterpreterOptionsDelete = procedure(options: Pointer); stdcall;
  TFLiteModelDelete = procedure(Model: Pointer); stdcall;
  TFLiteInterpreterInvoke = function(Interpreter: Pointer): TFLiteStatus; stdcall;
  TFLiteInterpreterOptionsAddDelegate = procedure(options: Pointer; delegate: Pointer); stdcall;
  TFLiteGpuDelegateV2Create = function(DelegateOptionsV2: Pointer): Pointer; stdcall;
  TFLiteGpuDelegateV2Delete = procedure(delegate: Pointer); stdcall;
  TModifyGraphWithDelegate = function(delegate: Pointer): TFLiteStatus; stdcall;

var
  ModelCreateFromFile: TFLiteModelCreateFromFile = nil;
  InterpreterCreate: TFLiteInterpreterCreate = nil;
  InterpreterDelete: TfLiteInterpreterDelete = nil;
  InterpreterOptionsCreate: TFLiteInterpreterOptionsCreate = nil;
  InterpreterOptionsDelete: TFLiteInterpreterOptionsDelete = nil;
  InterpreterAllocateTensors: TFLiteInterpreterAllocateTensors = nil;
  InterpreterGetInputTensor: TFLiteInterpreterGetInputTensor = nil;
  InterpreterGetOutputTensor: TFLiteInterpreterGetOutputTensor = nil;
  TensorNumDims: TFLiteTensorNumDims = nil;
  TensorName: TFLiteTensorName = nil;
  TensorType: TFLiteTensorType = nil;
  TensorByteSize: TFLiteTensorByteSize = nil;

  InterpreterGetInputTensorCount: TFLiteInterpreterGetInputTensorCount = nil;
  InterpreterGetOutputTensorCount: TFLiteInterpreterGetOutputTensorCount = nil;
  TensorCopyFromBuffer: TFLiteTensorCopyFromBuffer = nil;
  TensorCopyToBuffer: TFLiteTensorCopyToBuffer = nil;
  InterpreterOptionsSetNumThreads: TFLiteInterpreterOptionsSetNumThreads = nil;

  ModelDelete: TFLiteModelDelete = nil;
  InterpreterInvoke: TFLiteInterpreterInvoke = nil;
  InterpreterOptionsAddDelegate: TFLiteInterpreterOptionsAddDelegate = nil;

procedure TTensorFlowLiteFMX.UnloadModel;
var
  i: Int32;
begin
  if Interpreter <> nil then
    InterpreterDelete(Interpreter);

  if InterpreterOptions <> nil then
    InterpreterOptionsDelete(InterpreterOptions);

  if Model <> nil then
    ModelDelete(Model);

  Model := nil;
  Interpreter := nil;
  ModelFileName := '';

  if Input.Count > 0 then
    for i := 0 to Input.Count - 1 do
      if Input.Tensors[i].Tensor <> nil then
        if Input.Tensors[i].DataSize > 0 then
          FreeMem(Input.Tensors[i].Data, Input.Tensors[i].DataSize);

  if Output.Count > 0 then
    for i := 0 to Output.Count - 1 do
      if Output.Tensors[i].Tensor <> nil then
        if Output.Tensors[i].DataSize > 0 then
          FreeMem(Output.Tensors[i].Data, Output.Tensors[i].DataSize);
end;

constructor TTensorFlowLiteFMX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Model := nil;
  Interpreter := nil;
  ModelFileName := '';

{$IFDEF MSWINDOWS}
  LibraryModule := WinApi.Windows.LoadLibrary(PWideChar(LibraryName));
{$ENDIF MSWINDOWS}
  if (LibraryModule = 0) then
  begin
    LibraryModule := WinApi.Windows.LoadLibrary(PWideChar('..\..\..\bin\windows\tflite.dll'));

    if (LibraryModule = 0) then
    begin
      raise ETensorFlowLiteFMXError.Create('LoadLibrary');
      Exit;
    end;
  end;

  ModelCreateFromFile := GetProcAddress(LibraryModule, 'TfLiteModelCreateFromFile');
  InterpreterOptionsCreate := GetProcAddress(LibraryModule, 'TfLiteInterpreterOptionsCreate');

  InterpreterAllocateTensors := GetProcAddress(LibraryModule, 'TfLiteInterpreterAllocateTensors');
  InterpreterOptionsDelete := GetProcAddress(LibraryModule, 'TfLiteInterpreterOptionsDelete');
  ModelDelete := GetProcAddress(LibraryModule, 'TfLiteModelDelete');
  InterpreterCreate := GetProcAddress(LibraryModule, 'TfLiteInterpreterCreate');
  InterpreterDelete := GetProcAddress(LibraryModule, 'TfLiteInterpreterDelete');
  InterpreterOptionsSetNumThreads := GetProcAddress(LibraryModule, 'TfLiteInterpreterOptionsSetNumThreads');
  InterpreterGetInputTensor := GetProcAddress(LibraryModule, 'TfLiteInterpreterGetInputTensor');
  InterpreterGetOutputTensor := GetProcAddress(LibraryModule, 'TfLiteInterpreterGetOutputTensor');
  InterpreterGetInputTensorCount := GetProcAddress(LibraryModule, 'TfLiteInterpreterGetInputTensorCount');
  InterpreterGetOutputTensorCount := GetProcAddress(LibraryModule, 'TfLiteInterpreterGetOutputTensorCount');
  TensorCopyFromBuffer := GetProcAddress(LibraryModule, 'TfLiteTensorCopyFromBuffer');
  TensorCopyToBuffer := GetProcAddress(LibraryModule, 'TfLiteTensorCopyToBuffer');
  InterpreterInvoke := GetProcAddress(LibraryModule, 'TfLiteInterpreterInvoke');
  TensorNumDims := GetProcAddress(LibraryModule, 'TfLiteTensorNumDims');
  TensorName := GetProcAddress(LibraryModule, 'TfLiteTensorName');
  TensorType := GetProcAddress(LibraryModule, 'TfLiteTensorType');
  TensorByteSize := GetProcAddress(LibraryModule, 'TfLiteTensorByteSize');

  if (@ModelCreateFromFile = nil) or (@InterpreterOptionsCreate = nil) or
    (@InterpreterAllocateTensors = nil) or (@InterpreterOptionsDelete = nil) or
    (@InterpreterGetInputTensorCount = nil) or (@InterpreterGetOutputTensorCount = nil) or
    (@ModelDelete = nil) or (@InterpreterCreate = nil) or (@InterpreterDelete = nil) or
    (@InterpreterOptionsSetNumThreads = nil) or (@InterpreterGetInputTensor = nil) or
    (@InterpreterGetOutputTensor = nil) or (@TensorCopyFromBuffer = nil) or
    (@TensorCopyToBuffer = nil) or (@InterpreterInvoke = nil) or
    (@TensorNumDims = nil) or (@TensorName = nil) or
    (@TensorType = nil) or (@TensorByteSize = nil)
  then
  begin
    raise ETensorFlowLiteFMXError.Create('GetProcAddress');
    Exit;
  end;
end;

destructor TTensorFlowLiteFMX.Destroy;
begin
  UnloadModel;

  if LibraryModule <> 0 then
  begin
    FreeLibrary(LibraryModule);
    LibraryModule := 0;
  end;

  inherited Destroy;
end;

function TTensorFlowLiteFMX.LoadModel(ModelPath: String; InterpreterThreadCount: Integer): TFLiteStatus;
var
  i: Int32;
  FStatus: TFLiteStatus;
begin
  Result := TFLiteError;

  UnloadModel;

{$IFDEF MSWINDOWS}
  Model := ModelCreateFromFile(PAnsiChar(AnsiString(ModelPath)));
  ModelFileName := ModelPath;
{$ENDIF MSWINDOWS}
  if Model = nil then
  begin
    Exit;
  end;

  InterpreterOptions := InterpreterOptionsCreate;

  if InterpreterOptions = nil then
  begin
    raise ETensorFlowLiteFMXError.Create('InterpreterOptionsCreate');
    Exit;
  end;

  InterpreterOptionsSetNumThreads(InterpreterOptions, InterpreterThreadCount);

  Interpreter := InterpreterCreate(Model, InterpreterOptions);

  if Interpreter = nil then
  begin
    raise ETensorFlowLiteFMXError.Create('InterpreterCreate');
    Exit;
  end;

  FStatus := InterpreterAllocateTensors(Interpreter);

  if FStatus <> TFLiteOk then
  begin
    raise ETensorFlowLiteFMXError.Create('InterpreterAllocateTensors');
    Exit;
  end;

  Input.Count := InterpreterGetInputTensorCount(Interpreter);
  Output.Count := InterpreterGetOutputTensorCount(Interpreter);

  SetLength(Input.Tensors, Input.Count);

  if Input.Count > 0 then
    for i := 0 to Input.Count - 1 do
    begin
      Input.Tensors[i].Tensor := InterpreterGetInputTensor(Interpreter, i);

      if Input.Tensors[i].Tensor <> nil then
      begin
        Input.Tensors[i].Name := AnsiString(PAnsiChar(TensorName(Input.Tensors[i].Tensor)));
        Input.Tensors[i].TensorType := TensorType(Input.Tensors[i].Tensor);
        Input.Tensors[i].NumberDimensions := TensorNumDims(Input.Tensors[i].Tensor);
        Input.Tensors[i].DataSize := TensorByteSize(Input.Tensors[i].Tensor);
        if Input.Tensors[i].DataSize > 0 then
          GetMem(Input.Tensors[i].Data, Input.Tensors[i].DataSize);
      end;
    end;

  SetLength(Output.Tensors, Output.Count);

  if Output.Count > 0 then
    for i := 0 to Output.Count - 1 do
    begin
      Output.Tensors[i].Tensor := InterpreterGetOutputTensor(Interpreter, i);

      if Output.Tensors[i].Tensor <> nil then
      begin
        Output.Tensors[i].Name := AnsiString(PAnsiChar(TensorName(Output.Tensors[i].Tensor)));
        Output.Tensors[i].TensorType := TensorType(Output.Tensors[i].Tensor);
        Output.Tensors[i].NumberDimensions := TensorNumDims(Output.Tensors[i].Tensor);
        Output.Tensors[i].DataSize := TensorByteSize(Output.Tensors[i].Tensor);
        if Output.Tensors[i].DataSize > 0 then
          GetMem(Output.Tensors[i].Data, Output.Tensors[i].DataSize);
      end;
    end;

  Result := TFLiteOk;
end;

function TTensorFlowLiteFMX.GetInputTensor(InputIndex: Int32): Pointer;
begin
  Result := InterpreterGetInputTensor(Interpreter, InputIndex);
end;

function TTensorFlowLiteFMX.GetOutputTensor(OutputIndex: Int32): Pointer;
begin
  Result := InterpreterGetOutputTensor(Interpreter, OutputIndex);
end;

function TTensorFlowLiteFMX.GetTensorSize(Tensor: Pointer): NativeUInt;
begin
  Result := 0;

  if (Tensor <> nil) then
    Result := TensorByteSize(Tensor);
end;

function TTensorFlowLiteFMX.SetInputData(InputTensor: Int32; InputData: Pointer; InputDataSize: NativeUInt): TFLiteStatus;
begin
  if (InputDataSize <> Input.Tensors[InputTensor].DataSize) then
  begin
    Result := TFLiteError;
    Exit;
  end;

  Result := TensorCopyFromBuffer(Input.Tensors[InputTensor].Tensor, InputData, InputDataSize);

  if Result = TFLiteOk then
    CopyMemory(Input.Tensors[InputTensor].Data, InputData, InputDataSize);
end;

function TTensorFlowLiteFMX.GetOutputData(OutputTensor: Int32; OutputData: Pointer; OutputDataSize: NativeUInt): TFLiteStatus;
begin
  if (OutputDataSize <> Output.Tensors[OutputTensor].DataSize) then
  begin
    Result := TFLiteError;
    Exit;
  end;

  Result := TensorCopyToBuffer(Output.Tensors[OutputTensor].Tensor, OutputData, OutputDataSize);

  if Result = TFLiteOk then
    CopyMemory(Output.Tensors[OutputTensor].Data, OutputData, OutputDataSize);
end;

function TTensorFlowLiteFMX.Inference(): TFLiteStatus;
begin
  Result := InterpreterInvoke(Interpreter);
end;

end.
