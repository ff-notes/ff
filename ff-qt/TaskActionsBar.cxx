#include "Builder.hxx"
#include "TaskActionsBar.hxx"


TaskActionsBar::TaskActionsBar(StorageHandle storageHandle, Note task) {
    auto storage = Storage{storageHandle};
    auto id = task.id;

    auto doneAction =
        addAction("Done and archive", [storage, id]{ storage.done(id); });
    if (task.isTracking) {
        doneAction->setEnabled(false);
        doneAction->setToolTip("External task must be done first.");
    }

    addWidget(
        New<QToolButton>()
        .setMenu(
            New<QMenu>()
            .addAction("Postpone", [storage, id]{ storage.postpone(id); })
        )
        .setPopupMode(QToolButton::InstantPopup)
        .setText("▼")
    );
}
