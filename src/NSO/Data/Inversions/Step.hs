module NSO.Data.Inversions.Step where

import NSO.Prelude
import NSO.Types.Inversion


stepDownload :: InversionRow Identity -> StepDownload
stepDownload row =
  fromMaybe StepDownloadNone $
    StepDownloaded <$> downloaded <|> StepDownloading <$> downloading
 where
  downloaded = do
    complete <- row.downloaded
    let datasets = row.downloadDatasets
    pure $ Downloaded{complete, datasets}

  downloading = do
    transfer <- row.downloadTaskId
    let datasets = row.downloadDatasets
    pure $ Downloading{transfer, datasets}


stepInvert :: InversionRow Identity -> StepInvert
stepInvert row = do
  fromMaybe StepInvertNone $
    (StepInverted <$> inverted) <|> (StepInverting <$> inverting)
 where
  inverted = do
    complete <- row.inverted
    commit <- row.inversionSoftware
    transfer <- row.uploadTaskId
    _ <- row.uploaded
    pure $ Inverted{transfer, complete, commit}

  inverting :: Maybe Inverting
  inverting = do
    -- we are only inverting if the previous step is complete
    _ <- row.downloaded
    pure $
      Inverting
        { transfer = row.uploadTaskId
        , commit = row.inversionSoftware
        }


stepGenerate :: InversionRow Identity -> StepGenerate
stepGenerate row = do
  fromMaybe StepGenerateNone $
    (StepGenerateError <$> generateError)
      <|> (StepGenerated <$> generated)
      <|> (StepGeneratingAsdf <$> generatingAsdf)
      <|> (StepGeneratingFits <$> generatingFits)
      <|> (StepGenerateTransferring <$> generateTransfer)
      <|> generateWaiting
 where
  generateError = row.invError

  generatingAsdf = do
    fits <- row.generatedFits
    transfer <- row.generateTaskId
    pure $ GeneratedFits{generatedFits = fits, transfer}

  generatingFits = do
    transferred <- row.generateTaskCompleted
    transfer <- row.generateTaskId
    pure $ GenTransferred{transferred, transfer}

  generated = do
    fits <- row.generatedFits
    asdf <- row.generatedAsdf
    transfer <- row.generateTaskId
    pure $ Generated{generatedFits = fits, generatedAsdf = asdf, transfer}

  generateWaiting = do
    _ <- row.inverted
    _ <- row.uploaded
    pure StepGenerateWaiting

  generateTransfer = row.generateTaskId


stepPublish :: InversionRow Identity -> StepPublish
stepPublish row =
  fromMaybe StepPublishNone $
    (StepPublished <$> row.published)
      <|> (StepPublishing <$> publishing)
 where
  publishing = do
    _ <- row.generatedAsdf
    _ <- row.generatedFits
    pure row.publishTaskId
