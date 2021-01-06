"use strict";

function IupTreeGetUserId(tree, id) {
//DEV/DOC this is effectively a null-op on the web browser.
//  let leaf = tree.querySelector('[data-tree-id="' + id + '"]');
//  let /*integer*/ userid = leaf.getAttribute('data-tree-id');
//  if (userid!==id) { crash("userid error?"); }
//  return userid;
    return id;
}

function IupTreeAddNodes(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*integer*/ id=-1) {

    function iupTreeSetNodeAttributes(leaf, /*sequence*/ attrs) {
        // (internal routine, leaf should be an li)
        for (let i=1; i<=length(attrs); i += 2) {
            let /*string*/ name = attrs[i];
//          if (!find(name,["sequence","COLOR","STATE","TITLE","TITLEFONT","TOGGLEVALUE","TOGGLEVISIBLE",
//                            "USERDATA","IMAGE","IMAGEEXPANDED","MARKED"])) {
            if (!find(name,["sequence","STATE","USERDATA"])) {
                crash("invalid tree attribute")
            }
            let /*object*/ v = attrs[i+1];
            if (string(v)) {
//              IupSetAttributeId(tree, name, id, v);
                if (name==="STATE") {
                    if (v==="COLLAPSED") {
                        leaf.classList.add('closed');
                    } else if (v==="EXPANDED") {
                        leaf.classList.remove('closed');
                    } else {
                        crash("?9/0");
                    }
                } else {
                    crash("?9/0");
                }
            } else if (name==="USERDATA" && integer(v)) {
//              IupTreeSetUserId(tree, id, v); // (maybe?)
                leaf.setAttribute('data-tree-id',v);
            } else {
                crash("?9/0");
            }
        }
    }

    function createElem(tagname, classname, innerhtml) {
        let res = document.createElement(tagname);
        res.classList.add(classname);
        if (innerhtml) { res.innerHTML = innerhtml; }
        return res;
    }

    function iupTreeAddNodesRec(/*Ihandle*/ tree, /*sequence*/ tree_nodes, /*bool*/ bRoot=true) {
        //
        // internal routine, the guts of IupTreeAddNodes, less the initial clear
        // Here, tree is the immediate parent that we're adding the [subtree] tree_nodes to.
        // In the root case, tree is the ul, otherwise tree is an li and the ul we need to
        // add the new leaf (li) to is the (first) child ul of that, create one if needed.
        // Every node in the tree is basically an li/span/a triplet, with the latter two
        // being the first two children of the li.
        //
        let label = string(tree_nodes)?tree_nodes:tree_nodes[1],
            leaf = createElem('li', 'leaf'),
            parentList = tree;
        leaf.appendChild(createElem('span','treeToggle'));
        leaf.appendChild(createElem('a', 'leafLabel', label));
        if (!bRoot) {
            let list = tree.querySelector('ul');
            if (!list) {
                list = tree.appendChild(createElem('ul', 'subtree'));
            }
            parentList = list;
        }
        parentList.appendChild(leaf);
        if (!string(tree_nodes)) {
            let children = ["sequence"],
                lench = length(children),
                lentn = length(tree_nodes);
            if (!atom(tree_nodes[lentn])) {
                children = tree_nodes[lentn];
                lench = length(children);
                leaf.classList.add(lench?'showExpander':'closed');
                leaf.classList.add('hasChildren');
            }
            for (let i=1; i<=lench; i += 1) {
                iupTreeAddNodesRec(leaf, children[i], false);
            }
            if (lentn===3) {
                iupTreeSetNodeAttributes(leaf, tree_nodes[2]);
            }
        }
    }

    function deleteChildren(node) {
        while (node.hasChildNodes()) {  
            node.removeChild(node.firstChild);
        }
    }

    if (id===-1) {
//      IupSetAttributeId(tree,"DELNODE",0,"ALL");  // (maybe...)
        deleteChildren(tree);
        iupTreeAddNodesRec(tree, tree_nodes, true);
    } else {
        // tree_nodes is actually just children
        let children = tree_nodes,
            lench = length(children);
        tree = tree.querySelector('[data-tree-id="' + id + '"]');
        let list = tree.querySelector('ul');
        if (list) { deleteChildren(list); }
        if (!lench) {
            tree.classList.remove('showExpander');
            tree.classList.add('closed');
        } else {
            for (let i=1; i<=lench; i += 1) {
                iupTreeAddNodesRec(tree, children[i], false);
            }
        }
    }
}

function IupTreeView(/*sequence*/ tree_nodes, /*atom*/ branchopen_cb=null, /*string*/ attributes="", /*sequence*/ args=[]) {
//
// Creates an IupTree from a recursive [callback] data structure.
//
// tree_nodes is as per IupTreeAddNodes()
// branchopen_cb can be used for deferred loading, or be null if tree_nodes is complete.
// you can also set branchopen_cb later, via IupSetCallback(tree,"BRANCHOPEN_CB",Icallback("branchopen_cb")) [DEV]
//
    let /*Ihandle*/ tree = createElem('ul', 'tree');

    tree.addEventListener('click', function(event) {
        let leaf = event.target, // an a.leafLabel or span.treeToggle
            tcl = leaf.classList,
            pcl = leaf.parentNode.classList; // always an li.leaf
        if (tcl.contains('leafLabel')) {
            // was selectTreeLeaf()
            if (!pcl.contains('selected')) {
                let prev = tree.querySelector('.selected');
                if (prev) { prev.classList.remove('selected'); }
                pcl.add('selected');
                let select_cb = tree.select_cb;
                if (select_cb) {
                    select_cb(leaf.innerText, ', ye ha');
                }
            }
        } else if (tcl.contains('treeToggle')) {
            let it = leaf.nextSibling.innerText; // the a.leafLabel
            if (pcl.contains('closed')) {
                pcl.remove('closed');
                let open_cb = tree.open_cb;
                if (open_cb) {
                    open_cb(it, ', wowee');
                }
                let branchopen_cb = tree.branchopen_cb;
                if (branchopen_cb) {
                    let id = leaf.parentNode.getAttribute('data-tree-id');
                    if (id) {
                        branchopen_cb(tree,id);
                    }
                }
            } else {
                pcl.add('closed');
                let close_cb = tree.close_cb;
                if (close_cb) {
                    close_cb(it, ', so there');
                }
            }
        }
    });

    if (branchopen_cb) {
//DEV we (also) need to support this, btw:
//      IupSetCallback(tree, "BRANCHOPEN_CB",  branchopen_cb);
        tree.branchopen_cb = branchopen_cb;
    }
    IupTreeAddNodes(tree, tree_nodes);
//DEV IupSetAttributes(tree,attributes,args); // maybe, MARKMULTIPLE
    return tree;
}

