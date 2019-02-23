#include "Builder.hxx"
#include "LinkButton.hxx"


LinkButton::LinkButton(QString text, QString url): super(text) {
    setFont(Make<QFont>().setBold(false).setUnderline(true));
    setIcon(QIcon());
    setPalette(
        Make<QPalette>(palette())
        .setColor(QPalette::ButtonText, palette().color(QPalette::Link))
    );
    setToolTip(url);
    connect(this, &self::clicked, [=]{
        /// \todo(2019-02-23, cblp) Yandex.Browser on macOS doesn't open a tab
        /// via QDesktopServices::openUrl
        #ifdef __APPLE__
            QProcess::execute("open", {url});
        #else
            QDesktopServices::openUrl(QUrl(url));
        #endif
    });
    /// \todo(2019-02-12, cblp) Allow to copy link via context menu
}
