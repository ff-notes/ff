#include "Model.hxx"


int Model::columnCount(const QModelIndex &) const {
    return 1;
}


QVariant Model::data(const QModelIndex &, int) const {
    return {};
}


int Model::rowCount(const QModelIndex & parent) const {
    return parent.isValid() ? 0 : tasks.size();
}


Model::Model() {
}


QModelIndex Model::index(int row, int column, const QModelIndex & parent) const
{
    if (parent.isValid() /* leaf */ || column != 0)
        return {};
    return createIndex(row, column);
}


QModelIndex Model::parent(const QModelIndex &) const {
    return {};
}


Note Model::task(QModelIndex index) {
    return tasks[index.row()];
}


void Model::upsertTask(Note task) {
    auto it = taskIndex.find(task.id);
    if (it == taskIndex.end()) {
        // insert a new row to the root
        size_t row = tasks.size();
        beginInsertRows(QModelIndex(), row, row);
        tasks.push_back(task);
        taskIndex.emplace(task.id, row);
        endInsertRows();
    } else {
        // TODO update
    }
}
