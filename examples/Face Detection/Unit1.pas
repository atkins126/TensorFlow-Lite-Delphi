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
    ComboBox1: TComboBox;
    Label1: TLabel;
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

  // AMD Ryzen 5 3500X, Windows 10, 8 threads, CPU
const
  // tflite models with static input shape
  // 320, 6300 - image with 320x320 pixels, inference time 0.031 sec, 32 frame per sec, good detection quality, perfect for selfies
  // 480, 14175 - image with 480x480 pixels, inference time 0.064 sec, 15 frame per sec, good detection quality
  // 640, 25200 - image with 640x640 pixels, inference time 0.109 sec, 9 frame per sec, high quality detection
  // 800, 39375 - image with 800x800 pixels, inference time 0.157 sec, 6 frame per sec, best quality detection

  FaceDetectionInputSize = 800;
  FaceDetectionOutputSize = 39375;

type
  PInputDataFaceDetection = ^TInputDataFaceDetection;
  TInputDataFaceDetection = array [0 .. FaceDetectionInputSize - 1] of array [0 .. FaceDetectionInputSize - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceDetection = ^TOutputDataFaceDetection;
  TOutputDataFaceDetection = array [0 .. FaceDetectionOutputSize - 1] of array [0 .. 6 - 1] of Float32;

type
  TFace = record
    Rect: TRectF;
    Probability: Float32;
  end;

type
  TFaceList = record
    Faces: array of TFace;
    Count: Int32;
  end;

var
  HideProbability: Boolean = False;

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := FaceDetectionInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := FaceDetectionInputSize;

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
          RectF(0, 0, FaceDetectionInputSize, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / FaceDetectionInputSize)),
          1, False);
      end
      else
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / FaceDetectionInputSize), FaceDetectionInputSize),
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

function GetFaceList(Probability: Float32; NMS: Integer; OutputData: TOutputDataFaceDetection): TFaceList;
var
  i, X, Y: DWORD;
  FListNMS: array of TFace;
  FRect: TRectF;
  FExist: Boolean;
begin
  SetLength(Result.Faces, 0);
  Result.Count := 0;

  SetLength(FListNMS, 0);

  Y := 0;

  while True do
  begin
    if Y > FaceDetectionOutputSize then
      Break;

    if (OutputData[Y][4] >= Probability) and (OutputData[Y][4] <= 1.0) then
    begin
      SetLength(FListNMS, Length(FListNMS) + 1);
      FListNMS[Length(FListNMS) - 1].Rect.Left := ((FaceDetectionInputSize * OutputData[Y][0]) - ((FaceDetectionInputSize * OutputData[Y][2]) / 2));
      FListNMS[Length(FListNMS) - 1].Rect.Top := ((FaceDetectionInputSize * OutputData[Y][1]) - ((FaceDetectionInputSize * OutputData[Y][3]) / 2));
      FListNMS[Length(FListNMS) - 1].Rect.Width := (FaceDetectionInputSize * OutputData[Y][2]);
      FListNMS[Length(FListNMS) - 1].Rect.Height := (FaceDetectionInputSize * OutputData[Y][3]);
      FListNMS[Length(FListNMS) - 1].Probability := OutputData[Y][4];

      if Length(FListNMS) > 0 then
      begin
        for i := Y - NMS to Y + NMS - 1 do
        begin
          if (OutputData[i][4] > OutputData[Y][4]) then
          begin
            FRect.Left := ((FaceDetectionInputSize * OutputData[i][0]) - ((FaceDetectionInputSize * OutputData[i][2]) / 2));
            FRect.Top := ((FaceDetectionInputSize * OutputData[i][1]) - ((FaceDetectionInputSize * OutputData[i][3]) / 2));
            FRect.Width := (FaceDetectionInputSize * OutputData[i][2]);
            FRect.Height := (FaceDetectionInputSize * OutputData[i][3]);

            for X := 0 to Length(FListNMS) - 1 do
            begin
              if IntersectRect(FListNMS[X].Rect, FRect) then
              begin
                if (FaceDetectionInputSize * OutputData[i][0] > FListNMS[X].Rect.Left) and
                  (FaceDetectionInputSize * OutputData[i][0] < FListNMS[X].Rect.Right) and
                  (FaceDetectionInputSize * OutputData[i][1] > FListNMS[X].Rect.Top) and
                  (FaceDetectionInputSize * OutputData[i][1] < FListNMS[X].Rect.Bottom) then
                begin
                  FListNMS[X].Rect.Left := FRect.Left;
                  FListNMS[X].Rect.Top := FRect.Top;
                  FListNMS[X].Rect.Width := FRect.Width;
                  FListNMS[X].Rect.Height := FRect.Height;
                  FListNMS[X].Probability := OutputData[i][4];
                end;
              end;
            end;
          end;
        end;
      end;
    end;

    Inc(Y);
  end;

  if Length(FListNMS) > 0 then
  begin
    for Y := 0 to Length(FListNMS) - 1 do
    begin
      FExist := False;

      if (Length(Result.Faces) > 0) then
      begin
        for i := 0 to Length(Result.Faces) - 1 do
        begin
          if IntersectRect(Result.Faces[i].Rect, FListNMS[Y].Rect) then
          begin
            if (Abs(Result.Faces[i].Rect.Left - FListNMS[Y].Rect.Left) < 5) or
              (Abs(Result.Faces[i].Rect.Top - FListNMS[Y].Rect.Bottom) < 5) then
            begin
              Result.Faces[i].Rect.Left := FListNMS[Y].Rect.Left;
              Result.Faces[i].Rect.Top := FListNMS[Y].Rect.Top;
              Result.Faces[i].Rect.Width := FListNMS[Y].Rect.Width;
              Result.Faces[i].Rect.Height := FListNMS[Y].Rect.Height;
              Result.Faces[i].Probability := FListNMS[Y].Probability;

              FExist := True;
              Break;

            end;
          end;
        end;
      end;

      if (FExist = False) and (FListNMS[Y].Probability >= Probability) then
      begin
        SetLength(Result.Faces, Length(Result.Faces) + 1);
        Result.Faces[Length(Result.Faces) - 1].Rect.Left := FListNMS[Y].Rect.Left;
        Result.Faces[Length(Result.Faces) - 1].Rect.Top := FListNMS[Y].Rect.Top;
        Result.Faces[Length(Result.Faces) - 1].Rect.Width := FListNMS[Y].Rect.Width;
        Result.Faces[Length(Result.Faces) - 1].Rect.Height := FListNMS[Y].Rect.Height;
        Result.Faces[Length(Result.Faces) - 1].Probability := FListNMS[Y].Probability;

        Result.Count := Length(Result.Faces);
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  i, X, Y: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceDetection;
  FOutputData: POutputDataFaceDetection;
  FStatus: TFLiteStatus;
  FFaceList: TFaceList;
  FRect: TRectF;
  FLabel: String;
  FTickCountInference, FTickCountNMS: Int64;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      FTickCountInference := TThread.GetTickCount;

      GetMem(FInputData, FaceDetection.Input.Tensors[0].DataSize);
      try
        for Y := 0 to FaceDetectionInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to FaceDetectionInputSize - 1 do
          begin
            FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R / 255);
            FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G / 255);
            FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B / 255);
          end;
        end;

        FStatus := FaceDetection.SetInputData(0, FInputData, FaceDetection.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

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

      GetMem(FOutputData, FaceDetection.Output.Tensors[0].DataSize);
      try
        FStatus := FaceDetection.GetOutputData(0, FOutputData, FaceDetection.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        FTickCountNMS := TThread.GetTickCount;

        FFaceList := GetFaceList(StrToFloat(ComboBox1.Items[ComboBox1.ItemIndex]), 40, FOutputData^);

        ImageMain.Bitmap.Canvas.BeginScene;
        try
          FRect.Width := Screen.Width;
          FRect.Height := Screen.Height;
          ImageMain.Bitmap.Canvas.Font.Size := 20;
          ImageMain.Bitmap.Canvas.Font.Family := 'Microsoft Sans Serif';

          if FFaceList.Count > 0 then
          begin
            FLabel := 'Inference time: ' + FloatToStr((TThread.GetTickCount - FTickCountInference) / 1000) + ' sec, NMS time: ' +
              FloatToStr((TThread.GetTickCount - FTickCountNMS) / 1000) + ' sec' +
              ', face count: ' + IntToStr(FFaceList.Count);

            ImageMain.Bitmap.Canvas.Fill.Color := TAlphaColorRec.Black;
            ImageMain.Bitmap.Canvas.FillText(
              RectF(5, 5, ImageMain.Width, ImageMain.Height), FLabel, False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
            ImageMain.Bitmap.Canvas.Fill.Color := TAlphaColorRec.White;
            ImageMain.Bitmap.Canvas.FillText(
              RectF(6, 6, ImageMain.Width, ImageMain.Height), FLabel, False, 1, [], TTextAlign.Leading, TTextAlign.Leading);

            ImageMain.Bitmap.Canvas.Font.Size := 14;
            ImageMain.Bitmap.Canvas.MeasureText(FRect, 'Wq0,000', False, [], TTextAlign.Leading, TTextAlign.Leading);
            ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
            ImageMain.Bitmap.Canvas.Stroke.Thickness := 2;

            for i := 0 to FFaceList.Count - 1 do
            begin
              ImageMain.Bitmap.Canvas.DrawRect(FFaceList.Faces[i].Rect, 0, 0, AllCorners, 1);

              if not HideProbability then
                ImageMain.Bitmap.Canvas.FillText(
                  RectF(FFaceList.Faces[i].Rect.Left, FFaceList.Faces[i].Rect.Top - FRect.Height, FFaceList.Faces[i].Rect.Right + FRect.Width, FFaceList.Faces[i].Rect.Bottom),
                  Copy(FloatToStr(FFaceList.Faces[i].Probability), 1, 4),
                  False, 1, [], TTextAlign.Leading, TTextAlign.Leading);
            end;
          end;
        finally
          ImageMain.Bitmap.Canvas.EndScene;
        end;

      finally
        FreeMem(FOutputData);
      end;
    finally
      ImageList.Source[1].MultiResBitmap[0].Bitmap.Unmap(FBitmapData);
    end;
  end;
end;

procedure TForm1.ComboBox2Change(Sender: TObject);
begin
  FaceDetection.LoadModel('..\..\..\..\models\face_detection\face_detection_800.tflite', StrToInt(ComboBox2.Items[ComboBox2.ItemIndex]));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FaceDetection := TTensorFlowLiteFMX.Create(Self);

  // ModelPath: String, ThreadCount: Integer
  FaceDetection.LoadModel('..\..\..\..\models\face_detection\face_detection_800.tflite', 8);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceDetection.Destroy;
end;

end.
