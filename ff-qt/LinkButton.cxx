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
        QDesktopServices::openUrl(QUrl(url));
    });
    /// \todo(2019-02-12, cblp) Allow to copy link via context menu
}
