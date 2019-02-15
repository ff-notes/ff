#include "DateComponent.hpp"
#include "proxy.hpp"
#include "TaskWidget.hpp"


class TaskActionsButton: public QToolButton {
    using super = QToolButton;

public:

    TaskActionsButton (StorageHandle storageHandle, NoteId id) {
        auto storage = Storage{storageHandle};
        setText("⋮");
        setPopupMode(InstantPopup);
        {
            auto menu = new QMenu;
            menu->addAction("Postpone", [=]{ storage.postpone(id); });
            setMenu(menu);
        }
    }

};


class LinkButton: public QCommandLinkButton {
    using self  = LinkButton;
    using super = QCommandLinkButton;

public:

    LinkButton(QString text, QString url): super(text) {
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
};


QDate qDate(Date d) {
    return QDate(d.year, d.month, d.day);
}


TaskWidget::TaskWidget(QWidget * parent, StorageHandle storage, Note task):
    super(parent), label(new QLabel(QString::fromStdString(task.text)))
{
    auto box = new QVBoxLayout(this);
    box->addWidget(label);
    {
        auto row = new QHBoxLayout;
        row->addLayout(new DateComponent("Start:", qDate(task.start)));
        row->addLayout(new DateComponent("Deadline:", qDate(task.end)));
        row->addWidget(new TaskActionsButton(storage, task.id));
        row->addStretch();
        box->addLayout(row);
    }
    if (task.isTracking) {
        auto linkText = QString::fromStdString(
            task.track.provider + ": " + task.track.source + " #"
            + task.track.externalId
        );
        box->addWidget(
            new LinkButton(linkText, QString::fromStdString(task.track.url))
        );
    }
}
