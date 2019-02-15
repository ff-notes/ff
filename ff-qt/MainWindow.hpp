#ifndef ff_qt_MainWindow_hpp
#define ff_qt_MainWindow_hpp


#include <QtWidgets>

#include "proxy.hpp"
#include "TaskListWidget.hpp"


class MainWindow: public QMainWindow {
private:
    TaskListWidget * agenda;
public:
    MainWindow(StorageHandle storage);
    void upsertTask(Note note);
    void closeEvent(QCloseEvent *) override;
};


#endif // ff_qt_MainWindow_hpp
