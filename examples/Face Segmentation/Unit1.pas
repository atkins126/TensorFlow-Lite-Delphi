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
  FaceSegmentation: TTensorFlowLiteFMX;

const

  FaceSegmentationInputSize = 416;
  FaceSegmentationOutputSize = 416;

type
  PInputDataFaceSegmentation = ^TInputDataFaceSegmentation;
  TInputDataFaceSegmentation = array [0 .. FaceSegmentationInputSize - 1] of array [0 .. FaceSegmentationInputSize - 1] of array [0 .. 3 - 1] of Float32;

type
  POutputDataFaceSegmentation = ^TOutputDataFaceSegmentation;
  TOutputDataFaceSegmentation = array [0 .. FaceSegmentationOutputSize - 1] of array [0 .. 9 - 1] of Float32;

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
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Width := FaceSegmentationInputSize;
  ImageList.Source[1].MultiResBitmap[0].Bitmap.Height := FaceSegmentationInputSize;

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
          RectF(0, 0, FaceSegmentationInputSize, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / FaceSegmentationInputSize)),
          1, False);
      end
      else
      begin
        ImageList.Source[1].MultiResBitmap[0].Bitmap.Canvas.DrawBitmap(
          ImageList.Source[0].MultiResBitmap[0].Bitmap,
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width, ImageList.Source[0].MultiResBitmap[0].Bitmap.Height),
          RectF(0, 0, ImageList.Source[0].MultiResBitmap[0].Bitmap.Width / (ImageList.Source[0].MultiResBitmap[0].Bitmap.Height / FaceSegmentationInputSize), FaceSegmentationInputSize),
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
  i, X, Y, FPixel: DWORD;
  FColors: PAlphaColorArray;
  FBitmapData: TBitmapData;
  FInputData: PInputDataFaceSegmentation;
  FOutputData: POutputDataFaceSegmentation;
  FStatus: TFLiteStatus;
  FRect: TRectF;
  FLabel: String;
  FTickCountInference: Int64;
begin
  LoadImage;

  if ImageList.Source[1].MultiResBitmap.Count = 0 then
    Exit;

  if (ImageList.Source[1].MultiResBitmap[0].Bitmap.Map(TMapAccess.ReadWrite, FBitmapData)) then
  begin
    try
      FTickCountInference := TThread.GetTickCount;

      GetMem(FInputData, FaceSegmentation.Input.Tensors[0].DataSize);
      try
        for Y := 0 to FaceSegmentationInputSize - 1 do
        begin
          FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

          for X := 0 to FaceSegmentationInputSize - 1 do
          begin
            FInputData[Y][X][0] := (TAlphaColorRec(FColors[X]).R / 255);
            FInputData[Y][X][1] := (TAlphaColorRec(FColors[X]).G / 255);
            FInputData[Y][X][2] := (TAlphaColorRec(FColors[X]).B / 255);
          end;
        end;

        FStatus := FaceSegmentation.SetInputData(0, FInputData, FaceSegmentation.Input.Tensors[0].DataSize);
      finally
        FreeMem(FInputData);
      end;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('SetInputData Error');
        Exit;
      end;

      FStatus := FaceSegmentation.Inference;

      if FStatus <> TFLiteOk then
      begin
        ShowMessage('Inference Error');
        Exit;
      end;

      GetMem(FOutputData, FaceSegmentation.Output.Tensors[0].DataSize);
      try
        FStatus := FaceSegmentation.GetOutputData(0, FOutputData, FaceSegmentation.Output.Tensors[0].DataSize);

        if FStatus <> TFLiteOk then
          Exit;

        ImageMain.Bitmap.Canvas.BeginScene;
        try
          FPixel := 0;

          for Y := 0 to FaceSegmentationOutputSize - 1 do
          begin
            FColors := PAlphaColorArray(FBitmapData.GetScanline(Y));

            for X := 0 to FaceSegmentationOutputSize - 1 do
            begin
              // lip top
              if (FOutputData[FPixel][0] >= 0.5) and (FOutputData[FPixel][0] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Red;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // lip bottom
              if (FOutputData[FPixel][1] >= 0.5) and (FOutputData[FPixel][1] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Coral;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // right brow
              if (FOutputData[FPixel][2] >= 0.5) and (FOutputData[FPixel][2] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Green;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // left brow
              if (FOutputData[FPixel][3] >= 0.5) and (FOutputData[FPixel][3] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Lime;;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // right eye
              if (FOutputData[FPixel][4] >= 0.5) and (FOutputData[FPixel][4] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Yellow;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // left eye
              if (FOutputData[FPixel][5] >= 0.5) and (FOutputData[FPixel][5] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Fuchsia;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // mouth
              if (FOutputData[FPixel][6] >= 0.5) and (FOutputData[FPixel][6] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.Blue;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              // nose
              if (FOutputData[FPixel][7] >= 0.5) and (FOutputData[FPixel][7] <= 1) then
              begin
                ImageMain.Bitmap.Canvas.Stroke.Color := TAlphaColorRec.White;
                ImageMain.Bitmap.Canvas.DrawEllipse(RectF(X, Y, (X + 1), (Y + 1)), 0.25);
              end;

              Inc(FPixel);
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

procedure TForm1.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  SetPriorityClass(GetCurrentProcess, HIGH_PRIORITY_CLASS);

  FaceSegmentation := TTensorFlowLiteFMX.Create(Self);

  // ModelPath: String, ThreadCount: Integer
  FaceSegmentation.LoadModel('..\..\..\..\models\face_segmentation\face_segmentation_unet_416.tflite', 8);
{$ENDIF}
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FaceSegmentation.Destroy;
end;

end.
