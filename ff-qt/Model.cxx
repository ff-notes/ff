#include "Model.hxx"
#include "util.hxx"

using std::bind;
using std::placeholders::_1;
using std::placeholders::_2;
using std::vector;


struct Model::Impl {
    Model & q;
    std::vector<Note> tasks;
    using NoteIterator = vector<Note>::iterator;

    Impl(Model & q): q(q) {}

    bool lessByStartId(const Note & a, const Note & b) {
        auto aStart = qDate(a.start);
        auto bStart = qDate(b.start);
        return aStart == bStart ? a.id < b.id : aStart < bStart;
    }

    bool lessByEndStartId(const Note & a, const Note & b) {
        auto aEnd = qDate(a.end);
        auto bEnd = qDate(b.end);
        return aEnd.isValid()
            ?   bEnd.isNull() ||
                (aEnd == bEnd ? lessByStartId(a, b) : aEnd < bEnd)
            :   bEnd.isNull() && lessByStartId(a, b);
    }

    bool less(const Note & a, const Note & b) {
        return lessByEndStartId(a, b);
    }

    NoteIterator upper_bound(Note task) {
        return std::upper_bound(
            begin(tasks), end(tasks), task, bind(&Impl::less, this, _1, _2)
        );
    }

    void insertTask(Note task) {
        auto it = upper_bound(task);
        int row = it - begin(tasks);
        q.beginInsertRows({}, row, row);
        tasks.insert(it, task);
        q.endInsertRows();
    }

    void updateTask(NoteIterator src, Note task) {
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
    if (it == p->tasks.end()) {
        p->insertTask(task);
    } else {
        p->updateTask(it, task);
    }
}
