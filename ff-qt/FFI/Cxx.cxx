#include <QtCore>

#include "Cxx.hxx"
#include "../MainWindow.hxx"

MainWindow * proxy_main(std::string version, StorageHandle storagePtr) {
    int argc = 0;
    char argv0[] = "ff-qt";
    char * argv[] = {argv0, NULL};

    auto app = new QApplication(argc, argv);
    app->setOrganizationDomain("ff.cblp.su");
    app->setOrganizationName("ff");
    app->setApplicationName("ff");
    app->setApplicationVersion(QString::fromStdString(version));

    auto window = new MainWindow(storagePtr);
    window->show();
    return window;
}

int qApp_exec() {
    return qApp->exec();
}

void MainWindow_upsertTask(MainWindow * mainWindow, Note task) {
    mainWindow->upsertTask(task);
}
