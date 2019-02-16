#ifndef ff_qt_FFI_hxx
#define ff_qt_FFI_hxx


#include <functional>
#include <string>


struct StorageHandle { void * ptr; };

struct NoteId { std::string bytes; };

namespace std {
    template<>
    struct hash<NoteId> {
        typedef NoteId argument_type;
        typedef std::size_t result_type;
        result_type operator()(argument_type const & noteId)
            const // noexcept // TODO why noexcept doesn't work?
        {
            return std::hash<std::string>()(noteId.bytes);
        }
    };

    template<>
    struct equal_to<NoteId> {
        // constexpr // TODO why error?
        bool operator()(const NoteId & lhs, const NoteId & rhs) const {
            return lhs.bytes == rhs.bytes;
        }
    };
}

struct Date { int year, month, day; };

// \todo(2019-02-10, cblp) generate with ron-schema
struct Note {
    NoteId id;
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
    void c_postpone(StorageHandle, const char * noteId);
}

struct Storage {
    StorageHandle handle;
    void postpone(NoteId id) const { c_postpone(handle, id.bytes.c_str()); }
};

int qApp_exec();

MainWindow * proxy_main(std::string version, StorageHandle);

void MainWindow_upsertTask(MainWindow *, Note);


#endif // ff_qt_FFI_hxx
