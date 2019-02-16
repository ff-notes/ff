#include "Builder.hxx"
#include "MainWindow.hxx"


MainWindow::MainWindow(StorageHandle storage):
    agenda(new TaskListWidget(this, storage))
{
    // https://wiki.qt.io/Saving_Window_Size_State
    QSettings settings;
    restoreGeometry(settings.value("mainWindowGeometry").toByteArray());

    setCentralWidget(New<QTabWidget>().addTab(agenda, "Agenda"));
    setWindowTitle("ff");
    agenda->setFocus();

    restoreState(settings.value("mainWindowState").toByteArray());
}

void MainWindow::upsertTask(Note note) {
    // switch to this thread
    QMetaObject::invokeMethod(this, [=]{
        agenda->upsertTask(note);
    });
}

void MainWindow::closeEvent(QCloseEvent *) {
    // https://wiki.qt.io/Saving_Window_Size_State
    QSettings settings;
    settings.setValue("mainWindowGeometry", saveGeometry());
    settings.setValue("mainWindowState", saveState());
}
