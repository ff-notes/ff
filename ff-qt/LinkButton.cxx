#include "Builder.hxx"
#include "LinkButton.hxx"


LinkButton::LinkButton(QString text, QString url): super(text) {
    setCursor(Qt::PointingHandCursor);
    setFont(Make<QFont>(font()).setBold(false).setUnderline(true));
    setIcon(QIcon());
    setPalette(
        Make<QPalette>(palette())
        .setColor(QPalette::ButtonText, palette().color(QPalette::Link))
    );
    setToolTip(url);
    connect(this, &self::clicked, [=]{
        /// \todo(2019-02-23, cblp,
        /// browser@support.yandex.ru [Ticket#19022310562166345])
        /// Yandex.Browser on macOS doesn't open a tab via
        /// QDesktopServices::openUrl
        #ifdef __APPLE__
            QProcess::execute("open", {url});
        #else
            QDesktopServices::openUrl(QUrl(url));
        #endif
    });

    // context menu
    setContextMenuPolicy(Qt::ActionsContextMenu);
    {
        auto action = new QAction("Copy link address");
        connect(action, &QAction::triggered, [url]{
            qApp->clipboard()->setText(url);
        });
        addAction(action);
    }
}
