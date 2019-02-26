#include "Model.hxx"
#include "util.hxx"

using std::bind;
using std::placeholders::_1;
using std::placeholders::_2;
using std::tuple;
using std::vector;


struct Model::Impl {
    Model & q;
    vector<Note> tasks;
    using NoteIterator = vector<Note>::iterator;

    Impl(Model & q): q(q) {}

    enum TaskModeTag {
        Overdue,
        EndToday,
        EndSoon,
        Actual,
        Starting,
    };

    using TaskMode = tuple<TaskModeTag, unsigned>;

    TaskMode taskMode(QDate const & today, Note const & note) {
        auto const start = toQDate(note.start);
        auto const end   = toQDate(note.end);
        return
            end.isValid() ?
                ( end   <  today ? TaskMode{Overdue , today.daysTo(end)  }
                : end   == today ? TaskMode{EndToday, 0                  }
                : start <= today ? TaskMode{EndSoon , today.daysTo(end)  }
                :                  TaskMode{Starting, today.daysTo(start)} )
            : start <= today ?     TaskMode{Actual  , 0                  }
            :                      TaskMode{Starting, today.daysTo(start)} ;
    }

    bool naturalLess(Note const & a, Note const & b) {
        auto const today = QDate::currentDate();
        return taskMode(today, a) < taskMode(today, b);
    }

    bool less(Note const & a, Note const & b) {
        return naturalLess(a, b);
    }

    NoteIterator upper_bound(Note task) {
        return std::upper_bound(
            begin(tasks), end(tasks), task, bind(&Impl::less, this, _1, _2)
        );
    }

    void insert(Note task) {
        auto it = upper_bound(task);
        int row = it - begin(tasks);
        q.beginInsertRows({}, row, row);
        tasks.insert(it, task);
        q.endInsertRows();
    }

    void update(NoteIterator src, Note task) {
        bool const moveUp = src > begin(tasks) && less(task, src[-1]);
        bool const moveDown =
            src < end(tasks) && src + 1 < end(tasks) && less(src[1], task);
        if (moveUp || moveDown) {
            int rowSrc = src - begin(tasks);
            auto dst = upper_bound(task);
            int rowDst = dst - begin(tasks);
            q.beginMoveRows({}, rowSrc, rowSrc, {}, rowDst);
            if (moveUp) {
                tasks.erase(src);
                tasks.insert(begin(tasks) + rowDst, task);
            } else if (moveDown) {
                tasks.insert(dst, task);
                tasks.erase(begin(tasks) + rowSrc);
                --rowDst;
            }
            q.endMoveRows();
            auto ix = q.index(rowDst);
            emit q.dataChanged(ix, ix);
        } else {
            // update in place
            *src = task;
            auto ix = q.index(src - begin(tasks));
            emit q.dataChanged(ix, ix);
        }
    }

    void remove(NoteIterator it) {
        int row = it - begin(tasks);
        q.beginRemoveRows({}, row, row);
        tasks.erase(it);
        q.endRemoveRows();
    }
};


int Model::columnCount(const QModelIndex &) const {
    return 1;
}


QVariant Model::data(const QModelIndex &, int) const {
    return {};
}


int Model::rowCount(const QModelIndex & parent) const {
    return parent.isValid() ? 0 : p->tasks.size();
}


Model::Model(): p(new Impl(*this)) {}


QModelIndex
Model::index(int row, int column, const QModelIndex & parent) const {
    if (parent.isValid() /* leaf */ || column != 0)
        return {};
    return createIndex(row, column);
}


QModelIndex Model::parent(const QModelIndex &) const {
    return {};
}


Note Model::task(QModelIndex index) {
    return p->tasks[index.row()];
}


void Model::upsertTask(Note task) {
    auto it = find_if(
        begin(p->tasks),
        end(p->tasks),
        [task](Note t) -> bool { return t.id == task.id; }
    );
    if (task.isActive) {
        if (it == p->tasks.end()) {
            p->insert(task);
        } else {
            p->update(it, task);
        }
    } else {
        if (it != p->tasks.end()) {
            p->remove(it);
        }
    }
}
