var ws = {};
ws.activeProjectId ="";
ws.activeTaskId = "";
ws.activeNodesId = new Array();


function saveOpenNodes(activeProject){
    ws.activeNodesId.length=0;
    if (activeProject!=="") ws.activeNodesId.push('project_'+activeProject);
    $('.collapsable').each(function(){if(this.getAttribute('id')) ws.activeNodesId.push(this.getAttribute('id'));});
  }

function loadOpenNodes(){
 for (var key in ws.activeNodesId)
  {
    if (document.getElementById(ws.activeNodesId[key]))
        document.getElementById(ws.activeNodesId[key]).setAttribute('class', 'collapsable');
    else ws.activeNodesId.splice(key,1);
  }
  if (ws.activeProjectId){
      if (document.getElementById(ws.activeProjectId))
          document.getElementById(ws.activeProjectId).setAttribute('class', 'collapsable');
      else ws.activeProjectId = '';
  }
  if (ws.activeTaskId) {
      if (document.getElementById(ws.activeTaskId))
          document.getElementById(ws.activeTaskId).setAttribute('class', 'collapsable active');
      else ws.activeTaskId = '';
  }
}

// - utils
function removeNodeById(nodeId)
{
   var node = document.getElementById(nodeId);
   if (node) node.parentNode.removeChild(node);
}

function getIcon(type){
    var icon;
    switch (type)
    {
        case 'add' : icon = "ui-icon-plus";  break;
        case 'ds_add' : icon = "ui-icon-plus";  break;
        case 'ds_edit' : icon = "ui-icon-wrench";  break;
        case 'link_add' : icon = "ui-icon-plus";  break;
        case 'link_edit' : icon = "ui-icon-wrench";  break;
        case 'link_spec': icon = "ui-icon-shuffle"; break;
        case 'delete' : icon = "ui-icon-trash";  break;
        case 'import': icon = "ui-icon-arrowthickstop-1-s"; break;
        case 'export': icon = "ui-icon-arrowthick-1-ne"; break;
    }
    return icon;
}

// -- display functions --
function addLeaf(leaf, parent, desc)
{
          if (leaf){
             var leaf_ul = document.createElement("ul");
                 parent.appendChild(leaf_ul);
             var leaf_li = document.createElement("li");
                 leaf_ul.appendChild(leaf_li);
             var leaf_span = document.createElement("span");
                 leaf_span.setAttribute('class', 'file');
                 leaf_span.innerHTML = desc+leaf;
                 leaf_li.appendChild(leaf_span);
          }
}
// using jquery.ui icons
function addAction(type, label, desc, action, parent, activeProject, enabled)
{
    var icon = getIcon(type);
    var action_button = document.createElement("button");
    if (enabled){
            action_button.setAttribute('onclick', 'saveOpenNodes(\''+activeProject+'\');'+action);
            action_button.setAttribute('class','action');
            $(action_button).button({
                icons: {
                    primary: icon
                },
                label: label
            })
    } else {
            action_button.setAttribute('class','ui-icon ui-state-disabled '+icon );
    }
    action_button.setAttribute('title', desc);
    parent.appendChild(action_button);  
}

function addDataSource(jsonDataSource,projectNode,projectName)
{
    var ds_ul = document.createElement("ul");
        projectNode.appendChild(ds_ul);
    var ds_li = document.createElement("li");
        ds_li.setAttribute('id','datasource_'+projectName+'_'+jsonDataSource.name);
        ds_li.setAttribute('class', 'closed');
        ds_ul.appendChild(ds_li);
    var ds_span = document.createElement("span");
        ds_span.setAttribute('class', 'source');
        ds_li.appendChild(ds_span);

    var ds_label= document.createElement("span");
        ds_label.setAttribute('class', 'label');
        ds_label.innerHTML = jsonDataSource.name;
        ds_span.appendChild(ds_label);

    var ds_actions = document.createElement("div");
        ds_actions.setAttribute('class', 'actions');
        ds_span.appendChild(ds_actions);
     addAction('ds_edit', "Edit", "Edit data source "+jsonDataSource.name,"editSourceTask('"+projectName+"','"+ jsonDataSource.name+"')",ds_actions,projectName,true);
     addAction('delete', "Remove", "Remove DataSource "+jsonDataSource.name,"confirmDelete('removeSourceTask','"+projectName+"','"+jsonDataSource.name+"')",ds_actions,projectName,true);

    for(var p in jsonDataSource.params) {
      var param = jsonDataSource.params[p];
      addLeaf(param.value,ds_li, param.key + ': ');
    }
}

function addLinkingTask(jsonLinkingTask,projectNode,projectName)
{
    var lt_ul = document.createElement("ul");
        projectNode.appendChild(lt_ul);
    var lt_li = document.createElement("li");
        lt_li.setAttribute('id','linkingtask_'+projectName+'_'+jsonLinkingTask.name);
        lt_li.setAttribute('class', 'closed');
        lt_ul.appendChild(lt_li);
    var lt_span = document.createElement("span");
        lt_span.setAttribute('class', 'link');
        lt_li.appendChild(lt_span);

    var lt_label = document.createElement("span");
        lt_label.setAttribute('class', 'label');
        lt_label.innerHTML = jsonLinkingTask.name;
        lt_span.appendChild(lt_label);

    var lt_actions = document.createElement("div");
        lt_actions.setAttribute('class', 'actions');
        lt_span.appendChild(lt_actions);
    addAction('link_edit',"Metadata", "Edit metadata","editLinkingTask('"+projectName+"','"+ jsonLinkingTask.name+"')",lt_actions,projectName,true);
    addAction('link_spec',"Open", "Edit link specification "+jsonLinkingTask.name,"openLinkingTask('"+projectName+"','"+ jsonLinkingTask.name+"')",lt_actions,projectName,true);
    addAction('delete',"Remove", "Remove task "+jsonLinkingTask.name,"confirmDelete('removeLinkingTask','"+projectName+"','"+ jsonLinkingTask.name+"')",lt_actions,projectName,true);

    addLeaf(jsonLinkingTask.source,lt_li, 'source: ');
    addLeaf(jsonLinkingTask.target,lt_li, 'target: ');
    addLeaf(jsonLinkingTask.sourceDataset,lt_li, 'source dataset: ');
    addLeaf(jsonLinkingTask.targetDataset,lt_li, 'target dataset: ');
    addLeaf(jsonLinkingTask.linkType,lt_li, 'link type: ');
}

// -- display the workspace as treeview
function updateWorkspace(obj){
        //var obj = jQuery.parseJSON('{"workspace": {   "project": [ {"name": "Project_1", "dataSource":[ {"name":"KEGG","url":"http://kegg.us"},{"name":"Wiki","url":"http://aba.com"}  ],  "linkingTask":[ {"name":"Gene","source":"KEGG","target":"Wiki","targetDataset":"rdf:type Wiki:Gene","sourceDataset":"rdf:type KEGG:gene"},{"name":"linkTask2","source":"KEGG","target":"Wiki"} ] }, {"name": "Project_2", "dataSource":[ {"name":"ABA","url":"http://aba.com"} ] }     ] } }');

        removeNodeById("div_tree");

        // root folder
        if (!document.getElementById("root-folder")){
            var rootFolder = document.createElement("div");
            rootFolder.setAttribute('id','root-folder');  
            document.getElementById("content").appendChild(rootFolder);
        }

        // new project button
        if (!document.getElementById("newproject")) {
            var newProj = document.createElement("div");
            newProj.setAttribute('id','newproject');
            addAction('add','Project', 'Create new project',"createProject()",newProj,"",true);
            document.getElementById("content").appendChild(newProj);
        }

        // import project button
        if (!document.getElementById("import")) {
            var importProj = document.createElement("div");
            importProj.setAttribute('id','import');
            addAction('import','Import', 'Import a project',"importProject()",importProj,"",true);
            document.getElementById("content").appendChild(importProj);
        }

        var tree = document.createElement("div");
                tree.id = "div_tree";

        var root = document.createElement("ul");      
            root.id = 'tree';
            root.setAttribute('class','filetree');
            tree.appendChild(root);

        // for each project                                  
        for (var p in obj.workspace.project) {
            var project = obj.workspace.project[p];
            var  proj = document.createElement("li");
                proj.setAttribute('class', 'closed');
                proj.setAttribute('id','project_'+project.name);
                root.appendChild(proj);
                var proj_span = document.createElement("span");
                    proj_span.setAttribute('class', 'folder');
                    proj.appendChild(proj_span);

                var proj_label = document.createElement("span");
                    proj_label.setAttribute('class', 'label');
                    proj_label.innerHTML = project.name;
                    proj_span.appendChild(proj_label);

                var proj_actions = document.createElement("div");
                    proj_actions.setAttribute('class', 'actions');
                    proj_span.appendChild(proj_actions);
                addAction('ds_add','Source', 'Add DataSource',"createSourceTask('"+project.name+"')",proj_actions,project.name,true);
                addAction('link_add','Task', 'Add linking task',"createLinkingTask('"+project.name+"')",proj_actions,project.name,true);
                addAction('export','Export', 'Export Project '+project.name,"exportProject('"+project.name+"')",proj_actions,project.name,true);
                addAction('delete','Remove', 'Remove project '+project.name,"confirmDelete('removeProject','"+project.name+"','')",proj_actions,'',true);


             // display dataSource
            for (var d in obj.workspace.project[p].dataSource)
            {
                addDataSource(project.dataSource[d],proj,project.name);
            }

            // display linkingTask
            for (var l in obj.workspace.project[p].linkingTask)
            {
                addLinkingTask(project.linkingTask[l],proj,project.name);
            }
        }

        document.getElementById("content").appendChild(tree);

        // uncollapse active project/task
        if(obj.workspace.activeProject){
             ws.activeProjectId = "project_"+obj.workspace.activeProject;
        }
        if(obj.workspace.activeTask)
            {
             var idPrefix = (obj.workspace.activeTaskType === "LinkingTask") ?  'linkingtask_' : 'datasource_';
             ws.activeTaskId = idPrefix+obj.workspace.activeProject+"_"+obj.workspace.activeTask;
            }
        if (ws.activeNodesId.length>0 || ws.activeTaskId || ws.activeProjectId)  loadOpenNodes();

        $("#tree").treeview();


    }

// - dialogs
function callAction(action,proj,res){
    // work-around : passing the action as string parameter -> the action would be invoked anyway
    switch (action)
        {
        case 'removeProject' :  removeProject(proj);  break;
        case 'removeSourceTask' :  removeSourceTask(proj,res);  break;
        case 'removeLinkingTask' : removeLinkingTask(proj,res); break;
        default : alert("Error: Action \'"+action+"\' not defined!");
        }
}

function confirmDelete(action,proj,res){
     var confirmDialog = document.createElement("div");
         confirmDialog.setAttribute('title','Delete');
         confirmDialog.setAttribute('id','dialog');
     var dialogText = document.createElement("p");
         dialogText.innerHTML = "Delete resource: "+proj+" "+res;
         confirmDialog.appendChild(dialogText);

     document.getElementById("content").appendChild(confirmDialog);

     $("#dialog").dialog({width: 400,
         modal: true,
         resizable: false,
         buttons: {
         "Yes, delete it": function() {callAction(action,proj,res); $(this).dialog("close");},
         "Cancel": function() {$(this).dialog("close");}
        }
        });
}
