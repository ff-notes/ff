#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <QtWidgets>

#include "proxy.hpp"
#include "TaskListWidget.hpp"


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

    void upsertTask(Note note) {
        // switch to this thread
        QMetaObject::invokeMethod(this, [=]{
            agenda->upsertTask(note);
        });
    }

    void closeEvent(QCloseEvent *) override {
        // https://wiki.qt.io/Saving_Window_Size_State
        QSettings settings;
        settings.setValue("mainWindowGeometry", saveGeometry());
        settings.setValue("mainWindowState", saveState());
    }
};


#endif // ff_qt_MainWindow_hpp
