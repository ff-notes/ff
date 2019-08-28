#ifndef ff_qt_MainWindow_hxx
#define ff_qt_MainWindow_hxx


#include <QtWidgets/QMainWindow>

#include "TaskListWidget.hxx"


class MainWindow: public QMainWindow {
private:
    TaskListWidget * agenda;
public:
    MainWindow(StorageHandle storage);
    void upsertTask(Note note);
    void closeEvent(QCloseEvent *) override;
};


#endif // ff_qt_MainWindow_hxx
