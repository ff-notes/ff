#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <map>
    using std::map;
#include <vector>
    using std::vector;
#include <experimental/optional>
    using std::experimental::optional;
#include <QtWidgets>


// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    QString id;
    QString text;
    QDate start;
    optional<QDate> end;
};


class DateComponent: public QHBoxLayout {
    using super = QHBoxLayout;

public:

    DateComponent(QString label, QDate date) {
        addWidget(new QLabel(label));
        {
            auto dateEdit = new QDateEdit(date);
            dateEdit->setCalendarPopup(true);
            dateEdit->setReadOnly(true);
            addWidget(dateEdit);
        }
    }
};


class TaskActionsButton: public QToolButton {
    using super = QToolButton;

public:

    TaskActionsButton () {
        setText("⋮");
        setPopupMode(InstantPopup);
        {
            auto menu = new QMenu;
            menu->addAction("Postpone", []{
                // runStorage h $ cmdPostpone taskId
            });
            setMenu(menu);
        }
    }

};


class TaskWidget: public QFrame {
    using super = QFrame;

private:

    QLabel * label;

public:

    TaskWidget(QWidget * parent, Note task):
        super(parent), label(new QLabel(task.text))
    {
        auto box = new QVBoxLayout(this);
        box->addWidget(label);
        {
            auto fields = new QHBoxLayout;
            fields->addLayout(new DateComponent("Start:", task.start));
            if (task.end)
                fields->addLayout(new DateComponent("Deadline:", *task.end));
            fields->addWidget(new TaskActionsButton);
            fields->addStretch();
            box->addLayout(fields);
        }
    }

};


class TaskListWidget: public QTreeView {
    using super = QTreeView;

public:

    TaskListWidget(QWidget * parent): super(parent) {
        setAlternatingRowColors(true);
        setHeaderHidden(true);
        setModel(new QStandardItemModel);
    }

    void addTask(Note task) {
        auto item = new QStandardItem;
        model().appendRow(item);
        auto wrap = new QWidget(this);
        {
            // trick: a transparent widget around an opaque one
            auto wrapBox = new QVBoxLayout(wrap);
            auto taskWidget = new TaskWidget(wrap, task);
            taskWidget->setAutoFillBackground(true);
            wrapBox->addWidget(taskWidget);
        }
        setIndexWidget(item->index(), wrap);
    }

    QStandardItemModel & model() const {
        return static_cast<QStandardItemModel &>(*super::model());
    }

};


class MainWindow: public QMainWindow {

private:

    TaskListWidget * agenda;

public:

    MainWindow(): agenda(new TaskListWidget(this)) {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        restoreGeometry(settings.value("mainWindowGeometry").toByteArray());

        auto tabs = new QTabWidget;
        tabs->addTab(agenda, "Agenda");
        setCentralWidget(tabs);
        setWindowTitle("ff");

        restoreState(settings.value("mainWindowState").toByteArray());
    }

    void addTask(Note note) { agenda->addTask(note); }

    void closeEvent(QCloseEvent * event) override {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        settings.setValue("mainWindowGeometry", saveGeometry());
        settings.setValue("mainWindowState", saveState());
    }
};


#endif // ff_qt_MainWindow_hpp
