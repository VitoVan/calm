$image = $Env:CALM_HOME + "build\calm.png"
$headlineText = 'This may take a few minutes'
$bodyText = 'Thanks for your patience'

$ToastImageAndText03 = [Windows.UI.Notifications.ToastTemplateType, Windows.UI.Notifications, ContentType = WindowsRuntime]::ToastImageAndText03
$TemplateContent = [Windows.UI.Notifications.ToastNotificationManager, Windows.UI.Notifications, ContentType = WindowsRuntime]::GetTemplateContent($ToastImageAndText03)
$TemplateContent.SelectSingleNode('//image[@id="1"]').SetAttribute('src', $image)
$TemplateContent.SelectSingleNode('//text[@id="1"]').InnerText = $headlineText
$TemplateContent.SelectSingleNode('//text[@id="2"]').InnerText = $bodyText
$AppId = '{1AC14E77-02E7-4E5D-B744-2EB1AE5198B7}\WindowsPowerShell\v1.0\powershell.exe'
[Windows.UI.Notifications.ToastNotificationManager]::CreateToastNotifier("Initialising CALM...").Show($TemplateContent)