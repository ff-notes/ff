#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <QtWidgets>

#include "proxy.hpp"


QDate qDate(Date d) {
    return QDate(d.year, d.month, d.day);
}


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;

public:

    DateComponent(QString label, QDate date) {
        const bool editable = false;
        if (editable) {
            /// \todo
            addWidget(new QLabel(label));
            auto dateEdit = new QDateEdit(date);
            dateEdit->setCalendarPopup(true);
            addWidget(dateEdit);
        } else { // not editable
            if (date.isValid()) {
                addWidget(new QLabel(label + " " + date.toString()));
            }
        }
    }
};


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


class TaskWidget: public QFrame {
    using super = QFrame;

private:

    QLabel * label;

public:

    TaskWidget(QWidget * parent, StorageHandle storage, Note task):
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

};


class TaskListWidget: public QTreeView {
    using super = QTreeView;

private:

    StorageHandle storage;

public:

    TaskListWidget(QWidget * parent, StorageHandle storage):
        super(parent), storage(storage)
    {
        setAlternatingRowColors(true);
        setHeaderHidden(true);
        setModel(new QStandardItemModel);
        {
            auto p = palette();
            p.setColor(QPalette::Highlight, QColor(179, 215, 255));
            setPalette(p);
        }
    }

    void upsertTask(Note task) {
        auto item = new QStandardItem;
        model().appendRow(item);
        auto taskWidget = new TaskWidget(this, storage, task);
        auto index = item->index();
        setIndexWidget(index, taskWidget);
        if (not currentIndex().isValid()) {
            setCurrentIndex(index);
        }
    }

    QStandardItemModel & model() const {
        return static_cast<QStandardItemModel &>(*super::model());
    }

};


class MainWindow: public QMainWindow {

private:

    TaskListWidget * agenda;

public:

    MainWindow(StorageHandle storage):
        agenda(new TaskListWidget(this, storage))
    {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        restoreGeometry(settings.value("mainWindowGeometry").toByteArray());

        auto tabs = new QTabWidget;
        tabs->addTab(agenda, "Agenda");
        setCentralWidget(tabs);
        setWindowTitle("ff");
        agenda->setFocus();

        restoreState(settings.value("mainWindowState").toByteArray());
    }

    void upsertTask(Note note) { agenda->upsertTask(note); }

    void closeEvent(QCloseEvent *) override {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        settings.setValue("mainWindowGeometry", saveGeometry());
        settings.setValue("mainWindowState", saveState());
    }
};


#endif // ff_qt_MainWindow_hpp
