#include "LinkButton.hxx"


LinkButton::LinkButton(QString text, QString url): super(text) {
    {
        auto f = QFont();
        f.setBold(false);
        f.setUnderline(true);
        setFont(f);
    }
    setIcon(QIcon());
    {
        auto p = palette();
        p.setColor(QPalette::ButtonText, p.color(QPalette::Link));
        setPalette(p);
    }
    setToolTip(url);
    connect(this, &self::clicked, [=]{
        QDesktopServices::openUrl(QUrl(url));
    });
    /// \todo(2019-02-12, cblp) Allow to copy link via context menu
}
