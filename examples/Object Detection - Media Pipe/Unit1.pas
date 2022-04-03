unit Unit1;

interface

uses
{$IFDEF MSWINDOWS}
  WinApi.Windows, Vcl.Graphics,
{$ENDIF}System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.Utils,
  System.ImageList, FMX.ImgList, FMX.ListBox;

type
  TForm1 = class(TForm)
    ImageMain: TImage;
    Button1: TButton;
    Button2: TButton;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    ComboBox2: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ComboBox2Change(Sender: TObject);
  private

  public
    procedure LoadImage;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}


uses TensorFlowLiteFMX;

var
  FaceDetection: TTensorFlowLiteFMX;

const
  ObjectDetectionInputSize = 300;
  ObjectDetectionOutputSize = 10;

type
  PInputDataObjectDetection = ^TInputDataObjectDetection;
  TInputDataObjectDetection = array [0 .. ObjectDetectionInputSize - 1] of array [0 .. ObjectDetectionInputSize - 1] of array [0 .. 3 - 1] of UInt8;

type
  POutputDataLocation = ^TOutputDataLocation;
  TOutputDataLocation = array [0 .. ObjectDetectionOutputSize - 1] of array [0 .. 4 - 1] of Float32;

type
  POutputDataScores = ^TOutputDataScores;
  TOutputDataScores = array [0 .. ObjectDetectionOutputSize - 1] of Float32;

procedure TForm1.LoadImage;
begin
{$IFDEF MSWINDOWS}
  if not FileExists(OpenDialog.FileName) then
    Exit;

  if ImageMain.MultiResBitmap.Count > 0 then
    ImageMain.MultiResBitmap[0].Free;

  ImageMain.MultiResBitmap.Add;

  if FileExists(OpenDialog.FileName) then
  begin
    if ImageList.Source[0].MultiResBitmap.Count > 0 then
      ImageList.Source[0].MultiResBitmap[0].Free;

    ImageList.Source[0].MultiResBitmap.Add;
    ImageList.Source[0].MultiResBitmap[0].Bitmap.LoadFromFile(OpenDialog.FileName);
  end;

  if ImageList.Source[1].MultiResBitmap.Count > 0 then
    ImageList.Source[1].MultiResBitmap[0].Free;

  ImageList.Source[1].MultiResBitmap.Add;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := ObjectDetectionInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := ObjectDetectionInputSize;

  ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
  try
    ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.BeginScene;
    try
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.Clear(TAlphaColorRec.Null);

      if ImageList.Source[0].MultiResBitmap[0].Bitmap.Width > ImageList.Source[0].MultiResBitmap[0].Bitmap.Height then
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ObjectDetectionInputSize, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / ObjectDetectionInputSize)),
          1, False);
      end
      else
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / ObjectDetectionInputSize), ObjectDetectionInputSize),
          1, False);
      end;

    finally
      ImageList.Source[0].MultiResBitmap[0].Bitmap.Canvas.EndScene;
    end;
  finally
    ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.EndScene;
  end;

  ImageMain.Bitmap.Assign(ImageList.Source[1].MultiResBitmap[0].Bitmap);

{$ENDIF MSWINDOWS}
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadImage;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, X, Y: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: TInputDataObjectDetection;
  FOutputDataLocation: TOutputDataLocation;
  FOutputDataScores: TOutputDataScores;
  FOutputDataClass: TOutputDataScores;
  FStatus: TFLiteStatus;
  FRect, FTextRect: TRectF;
  FLabelMap: TStringList;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  FLabelMap := TStringList.Create;
  FLabelMap.LoadFromFile('..\..\..\..\models\object_detection\media_pipe_object_detection_classes.txt');

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      for Y := 0 to ObjectDetectionInputSize - 1 do
      begin
        FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

        for X := 0 to ObjectDetectionInputSize - 1 do
        begin
          FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R);
          FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G);
          FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B);
        end;
      end;

      FStatus := FaceDetection.SetInputData(0, @FInputData, FaceDetection.Input.Tensors[0].DataSize);

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := FaceDetection.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      FStatus := FaceDetection.GetOutputData(0, @FOutputDataLocation, FaceDetection.Output.Tensors[0].DataSize);

      if FStatus <> TFLiteOk then
        Exit;

      FStatus := FaceDetection.GetOutputData(1, @FOutputDataClass, FaceDetection.Output.Tensors[2].DataSize);

      if FStatus <> TFLiteOk then
        Exit;

      FStatus := FaceDetection.GetOutputData(2, @FOutputDataScores, FaceDetection.Output.Tensors[1].DataSize);

      if FStatus <> TFLiteOk then
        Exit;

      ImageMain.Bitmap.Canvas.BeginScene;
      try
        FTextRect.Width := Screen.Width;
        FTextRect.Height := Screen.Height;
        ImageMain.Bitmap.Canvas.Font.Size := 12;
        ImageMain.Bitmap.Canvas.Font.Family := 'Microsoft Sans Serif';
        ImageMain.Bitmap.Canvas.MeasureText(FTextRect, 'Wq', False, [], TTextAlign.Leading, TTextAlign.Leading);
        ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
        ImageMain.Bitmap.Canvas.Stroke.Thickness := 2;

        for i := 0 to ObjectDetectionOutputSize - 1 do
        begin
          if FOutputDataScores[i] > 0.5 then
          begin
            FRect.Left := (ObjectDetectionInputSize * FOutputDataLocation[i][1]);
            FRect.Top := (ObjectDetectionInputSize * FOutputDataLocation[i][0]);
            FRect.Right := (ObjectDetectionInputSize * FOutputDataLocation[i][3]);
            FRect.Bottom := (ObjectDetectionInputSize * FOutputDataLocation[i][2]);

            ImageMain.Bitmap.Canvas.DrawRect(FRect, 0, 0, AllCorners, 1);

            ImageMain.Bitmap.Canvas.FillText(
              RectF(FRect.Left, FRect.Top - FTextRect.Height, 300, FRect.Bottom),
              FLabelMap.Strings[Round(FOutputDataClass[i]) + 1],
              False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
          end;
        end;

      finally
        ImageMain.Bitmap.Canvas.EndScene;
      end;

    finally
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Unmap(FBitmapData);
    end;
  end;

  FLabelMap.Free;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  FaceDetection.LoadModel('..\..\..\..\models\object_detection\media_pipe_object_detection.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FaceDetection := TTensorFlowLiteFMX.Create(Self);

  // ModelPath: String, ThreadCount: Integer
  FaceDetection.LoadModel('..\..\..\..\models\object_detection\media_pipe_object_detection.tflite', 4);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceDetection.Destroy;
end;

end.
