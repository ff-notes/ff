#ifndef ff_qt_FFI_hxx
#define ff_qt_FFI_hxx


#include <functional>
#include <string>


struct StorageHandle { void * ptr; };

typedef std::string NoteId;

struct Date { int year, month, day; };

// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    NoteId id;
    bool isActive;
    std::string text;
    Date start;
    Date end;
    bool isTracking;
    struct Track {
        std::string provider;
        std::string source;
        std::string externalId;
        std::string url;
    } track;
};

class MainWindow;

extern "C" {
    void c_done    (StorageHandle, const char * noteId);
    void c_postpone(StorageHandle, const char * noteId);
}

struct Storage {
    StorageHandle handle;
    void done    (NoteId id) const { c_done    (handle, id.c_str()); }
    void postpone(NoteId id) const { c_postpone(handle, id.c_str()); }
};

int qApp_exec();

MainWindow * proxy_main(std::string version, StorageHandle);

void MainWindow_upsertTask(MainWindow *, Note);


#endif // ff_qt_FFI_hxx
